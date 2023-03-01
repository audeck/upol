package cz.upol.jj2;

import cz.upol.jj2.XMLReaderWriters.XMLMessagesReaderWriter;
import cz.upol.jj2.XMLReaderWriters.XMLMessagesReaderWriter.MessageVector;
import cz.upol.jj2.XMLReaderWriters.XMLUserDataReaderWriter;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.lang.Thread;

/**
 * A simple messaging app server. The valid requests a client can implement are defined in
 * {@link cz.upol.jj2.MessageServer.MessageClientHandler.Request}.
 *
 * NOTE: The current storage solution are two XML files, loaded and saved by their XMLReaderWriters.
 * This does NOT scale well (though it helps that messages get deleted upon being read), but they're
 * somewhat easy to use and included in base Java (afaik).
 *
 * In a real world use case, both should be replaced by an actual database solution. This means
 * changing every method that uses any of 'saveUserData()', 'loadUserData()', 'saveMessages()' or
 * 'loadMessages()' to use methods that implement some API of some query language.
 *
 * NOTE #2: I am using the deprecated Thread.getId() method, since the Thread.threadId() is giving
 * me a "Cannot find symbol error".
 *
 * NOTE #3: I have no idea why, but I thought the lab02 assignment included threading for multiple
 * clients. Apparently this is not the case, but at least the server should work out of the box
 * for a single client according to the assignment anyway. You live and you learn. ¯\_(ツ)_/¯
 */
public class MessageServer {

  private static boolean running = true;
  private static final List<MessageClientHandler> threads = new ArrayList<>();

  public static void startServer(int port) throws Exception {
    ServerSocket serverSocket = new ServerSocket(port);
    serverSocket.setSoTimeout(100);  // serverSocket.accept() timeout
    System.out.println("[MessageServer]: Server started on port #" + port);

    while (isRunning()) {
      // Semi-non-blocking way of accepting new sockets
      try {
        MessageClientHandler newThread = new MessageClientHandler(serverSocket.accept());
        threads.add(newThread);
        newThread.start();
      } catch (SocketTimeoutException ignored) {}
    }

    // Indicate to all threads that they should stop
    for (MessageClientHandler thread : threads) {
      thread.stopThread();
    }

    // Close the server socket
    serverSocket.close();
    System.out.println("[MessageServer]: Server stopped");
  }


  // ------------------------- Client handler class -------------------------


  private static class MessageClientHandler extends Thread {

    private enum Request {
      CONNECT("CONNECT"),
      MESSAGE("MSG"),
      READ("READ"),
      LOGOUT("LOGOUT"),
      SERVER_STOP("STOP"),
      UNDEFINED("");

      private final String name;

      Request(String name) {
        this.name = name;
      }

      public String getName() {
        return name;
      }

      public static Request getRequest(String name) {
        for (Request rq : values()) {
          if (rq.getName().equalsIgnoreCase(name)) {
            return rq;
          }
        }
        return Request.UNDEFINED;
      }
    }

    private final Socket clientSocket;
    private BufferedReader input;
    private BufferedWriter output;
    private String loggedInAs;
    private boolean stopped;

    MessageClientHandler(Socket clientSocket) throws SocketException {
      this.clientSocket = clientSocket;
      this.clientSocket.setSoTimeout(100);  // 100 millisecond timeout (for readLine)
      loggedInAs = null;
      stopped = false;
      System.out.println(
          "[MessageServer]:" +
          " Client #" + clientSocket.getPort() + " connected on thread #" + this.getId()
      );
    }

    public void run() {
      try {
        input = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
        output = new BufferedWriter(new OutputStreamWriter(clientSocket.getOutputStream()));
        processRequests();
      } catch (Exception e) {
        e.printStackTrace();
      }
    }

    /** Waits for a request and then processes it. */
    private void processRequests() throws Exception {
      String text;

      while (!clientSocket.isClosed()) {
        // Close the socket and interrupt this thread
        if (stopped) {
          this.interruptThread();
          return;
        }

        // Read from the input stream; times out in 100 milliseconds,
        // allowing the above code to be executed every -//-
        try {
          text = input.readLine();
        } catch (SocketTimeoutException e) {
          continue;
        }

        System.out.println(
            "[MessageServer]: " +
                "Thread #" + this.getId() +  // this.threadId() is giving me Cannot find symbol
                " received '" + text + "'" +
                " from client #" + clientSocket.getPort()
        );

        // Handle input text
        String[] words = text.split(" ");
        String name = words[0];
        String[] args = Arrays.copyOfRange(words, 1, words.length);
        Request request = Request.getRequest(name);

        switch (request) {
          case CONNECT -> connect(args);
          case MESSAGE -> sendMessage(args);
          case READ -> readMessages();
          case LOGOUT -> logout();
          case SERVER_STOP -> stopServer();
          case UNDEFINED -> sendError();
        }
      }
    }

    /** Sends 'response' to the client (by means of OutputStream). Also sends a new-line char. */
    private void sendLine(String response) throws IOException {
      output.write(response);
      output.write('\n');
      output.flush();
    }

    private void sendError() throws IOException {
      System.out.println("[MessageServer]: Sent 'ERR'");
      sendLine("ERR");
      sendEndOfResponse();
    }

    private void sendOK() throws IOException {
      System.out.println("[MessageServer]: Sent 'OK'");
      sendLine("OK");
      sendEndOfResponse();
    }

    private void sendEndOfResponse() throws IOException {
      sendLine("--END-OF-RESPONSE--");
    }

    /**
     * Handles args.
     *
     * @see MessageClientHandler#connect(String, String)
     */
    private void connect(String[] args) throws Exception {
      // Check if there are enough arguments
      if (args.length < 2) {
        sendError();
        return;
      }

      // Read arguments
      String username = args[0];
      String password = String.join(" ", Arrays.copyOfRange(args, 1, args.length));

      // Call main method
      connect(username, password);
    }

    /**
     * Tries to log a user in:
     * <ul>
     *   <li>if the user isn't registered, this function registers them, logs them in and sends OK</li>
     *   <li>if the user is registered and password entered is correct, this function logs them in and sends OK</li>
     *   <li>if the user is registered and the password entered is incorrect, this function sends ERR</li>
     * </ul>
     */
    private void connect(String username, String password) throws Exception {
      // Check if the user isn't already logged in
      if (loggedInAs != null) {
        sendError();
        return;
      }

      // Get already saved password's hash for user (null if new user)
      Integer savedPassword = getUserPasswordHash(username);

      // Register user if new
      if (savedPassword == null) {
        register(username, password);
        savedPassword = password.hashCode();
      }

      // Handle login
      if (savedPassword == password.hashCode()) {
        loggedInAs = username;
        sendOK();
      } else {
        sendError();
      }
    }

    /** Finds and returns the user's password hash from storage, or null if not found. */
    private static Integer getUserPasswordHash(String name) throws Exception {
      Map<String, String> userData = loadUserData();

      for (var user : userData.entrySet()) {
        if (user.getKey().equalsIgnoreCase(name)) {
          return Integer.parseInt(user.getValue());
        }
      }

      return null;
    }

    /** Registers a new user by adding their user data (name, passwordHash) into storage. */
    private static void register(String name, String password) throws Exception {
      // Load user data
      Map<String, String> userData = loadUserData();

      // Store new pair
      userData.put(name, String.valueOf(password.hashCode()));

      // Store user data
      saveUserData(userData);
    }

    /**
     * Handles args.
     *
     * @see MessageClientHandler#sendMessage(String, String, String)
     */
    private void sendMessage(String[] args) throws Exception {
      // Check for correct args formatting
      if (!args[0].equalsIgnoreCase("for") || args.length < 3) {
        sendError();
        return;
      }

      // Read args
      String sender = loggedInAs;
      String recipient = args[1].replaceAll(":$", ""); // remove trailing colon if present
      String message = String.join(" ", Arrays.copyOfRange(args, 2, args.length));

      // Call main method
      sendMessage(sender, recipient, message);
    }

    /**
     * Sends a message by saving it into storage.
     */
    private void sendMessage(String sender, String recipient, String message) throws Exception {
      // Check if the user is logged in
      if (loggedInAs == null) {
        sendError();
        return;
      }

      // Load saved data
      Map<String, String> userData = loadUserData();
      List<Vector<String>> messages = loadMessages();

      // Check for a valid recipient
      if (!userData.containsKey(recipient.toLowerCase())) {
        sendError();
        return;
      }

      // Create message vector
      Vector<String> newMessage = new Vector<>();
      newMessage.add(sender);
      newMessage.add(recipient);
      newMessage.add(String.join(" ", message));

      // Add the vector to 'messages' and save them
      messages.add(newMessage);
      saveMessages(messages);

      // Send OK :)
      sendOK();
    }

    /** Reads and removes all messages addressed to the logged-in user from storage. */
    private void readMessages() throws Exception {
      // Check if a user is logged in
      if (loggedInAs == null) {
        sendError();
        return;
      }

      // Load messages from storage
      List<Vector<String>> messageData = loadMessages();

      // Send back all messages addressed to logged-in user
      for (Vector<String> message : messageData) {
        if (message.get(MessageVector.RECIPIENT.ordinal()).equalsIgnoreCase(loggedInAs)) {
          sendLine(
              "FROM " + message.get(MessageVector.SENDER.ordinal())
                  + ": " + message.get(MessageVector.MESSAGE.ordinal())
          );
        }
      }

      // Remove read messages (damn you concurrent modification!) and save them
      messageData =
          messageData.stream()
              .filter(m -> !m.get(MessageVector.RECIPIENT.ordinal()).equalsIgnoreCase(loggedInAs))
              .toList();
      saveMessages(messageData);

      // Send OK :)
      sendOK();
    }

    /** Logs the current user out, ends the connection and stops the current thread. */
    private void logout() throws IOException {
      sendOK();
      stopThread();
    }

    /** Marks this thread for termination. */
    private void stopThread() {
      this.stopped = true;
    }

    /** Closes the client socket and interrupts this thread. */
    private void interruptThread() throws IOException {
      clientSocket.close();
      this.interrupt();
      System.out.println("[MessageServer]: Thread #" + this.getId() + " interrupted.");
    }

    /** Signals to the server that it should stop. */
    private void stopServer() throws IOException {
      sendOK();
      setRunning(false);
    }
  }


  // ------------------------- DATA SAVE & LOAD -------------------------


  private static synchronized Map<String, String> loadUserData() throws Exception {
    Path userDataPath = Paths.get("xml/UserData.xml");
    XMLUserDataReaderWriter readerWriter = new XMLUserDataReaderWriter();
    return readerWriter.loadUserData(Files.newInputStream(userDataPath));
  }

  private static synchronized void saveUserData(Map<String, String> userData) throws Exception {
    Path userDataPath = Paths.get("xml/UserData.xml");
    XMLUserDataReaderWriter readerWriter = new XMLUserDataReaderWriter();
    readerWriter.saveUserData(Files.newOutputStream(userDataPath), userData);
  }

  private static synchronized List<Vector<String>> loadMessages() throws Exception {
    Path messagesPath = Paths.get("xml/Messages.xml");
    XMLMessagesReaderWriter messagesReaderWriter = new XMLMessagesReaderWriter();
    return messagesReaderWriter.loadMessages(Files.newInputStream(messagesPath));
  }

  private static synchronized void saveMessages(List<Vector<String>> messages) throws Exception {
    Path messagesPath = Paths.get("xml/Messages.xml");
    XMLMessagesReaderWriter messagesReaderWriter = new XMLMessagesReaderWriter();
    messagesReaderWriter.saveMessages(Files.newOutputStream(messagesPath), messages);
  }


  // ------------------------- Getters & Setters -------------------------


  private static synchronized void setRunning(boolean value) {
    running = value;
  }

  private static synchronized boolean isRunning() {
    return running;
  }


  // ------------------------- MAIN -------------------------


  // Testing on localhost w/ CLI
  public static void main(String[] args) throws Exception {
    startServer(4324);
  }
}
