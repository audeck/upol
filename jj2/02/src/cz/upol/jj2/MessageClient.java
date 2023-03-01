package cz.upol.jj2;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;
import java.util.Objects;
import java.util.Scanner;

public class MessageClient {
  private Socket clientSocket;
  private BufferedReader input;
  private BufferedWriter output;

  /**
   * Starts the client by creating a new client Socket and retrieves it's input and output streams.
   */
  public void startClient(InetAddress ip, int port) throws IOException {
    clientSocket = new Socket(ip, port);
    input = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
    output = new BufferedWriter(new OutputStreamWriter(clientSocket.getOutputStream()));
  }

  /**
   * Sends a request to this client's OutputStream. Also adds a new-line character (\n) to the end
   * of the request.
   */
  public void sendRequest(String request) throws IOException {
    output.write(request);
    output.write('\n');
    output.flush();
  }

  /** Returns this client's current InputStream line (retrieved using readLine()). */
  public String readResponse() throws IOException {
    return input.readLine();
  }

  /** Stops the client and cleans up. */
  public void stopClient() throws IOException {
    clientSocket.close();
  }

  // ------------------------- MAIN -------------------------

  // Testing in CLI on local network (can be run multiple times in parallel)
  public static void main(String[] args) throws IOException {
    // Setup client and input
    MessageClient client = new MessageClient();
    InetAddress ip = InetAddress.getLocalHost();
    client.startClient(ip, 4324);
    System.out.println("[MessageClient]: Listening for requests...");

    Scanner scanner = new Scanner(System.in);

    while (true) {
      // Get request from console and send it to the server
      String request = scanner.nextLine(); // blocking!
      client.sendRequest(request);

      try {
        // Print non-null responses until "--END-OF-RESPONSE--" is reached
        String response = client.readResponse();
        while (!Objects.equals(response, "--END-OF-RESPONSE--")) {
          if (response != null) {
            System.out.println(response);
          }

          // Update response
          response = client.readResponse();
        }
      } catch (SocketException e) {
        // In case of a SocketException, the server closed its socket (probably)
        System.out.println("[MessageClient]: Connection terminated by server.");
        client.stopClient();
        return;
      }

      // Stop the client on logout (optional)
      if (request.equalsIgnoreCase("logout")) {
        client.stopClient();
        System.out.println("[MessageClient]: Connection terminated by client.");
        break;
      }
    }
  }
}
