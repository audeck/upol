package cz.upol.jj2.server;

import cz.upol.jj2.utils.CodeTimer;
import cz.upol.jj2.utils.Sorter;
import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class SortingServer {
  private boolean running;
  private final ExecutorService executorService;
  private final ServerSocket serverSocket;
  private static final int SO_TIMEOUT_VALUE = 100;
  private static final boolean LOGGING_ENABLED = true;

  private void log(String logMessage) {
    if (LOGGING_ENABLED) {
      System.out.println(logMessage);
    }
  }

  /**
   * Creates and start a new localhost server on a specified port.
   * @param port the port specified
   */
  public SortingServer(int port) throws IOException {
    this.serverSocket = new ServerSocket(port);
    serverSocket.setSoTimeout(SO_TIMEOUT_VALUE);
    executorService = Executors.newCachedThreadPool();
    this.running = true;
    log("[SortingServer]: Server started on port #" + port + ".");
    this.run();
  }

  /** Main server loop */
  private void run() {
    while (isRunning()) {
      try {
        executorService.execute(new SortingClientHandler(serverSocket.accept()));
      } catch (SocketTimeoutException ignored) {
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    log("[SortingServer]: Server stopping.");
    executorService.shutdown();

    log("[SortingServer]: Awaiting thread pool termination.");
    try {
      executorService.awaitTermination(Long.MAX_VALUE, TimeUnit.MINUTES);
      log("[SortingServer]: Thread pool terminated.");
    } catch (InterruptedException e) {
      log("[SortingServer]: Skipped waiting for thread pool termination.");
      throw new RuntimeException(e);
    }

    log("[SortingServer]: Server stopped.");
  }

  private class SortingClientHandler implements Runnable {
    private final Socket clientSocket;
    private final BufferedReader input;
    private final BufferedWriter output;
    private boolean finished;

    /**
     * Creates a new handler that handles communication with a new client on a given socket.
     * @param clientSocket the given client's socket
     */
    SortingClientHandler(Socket clientSocket) throws IOException {
      this.clientSocket = clientSocket;
      this.clientSocket.setSoTimeout(SO_TIMEOUT_VALUE);
      this.input = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
      this.output = new BufferedWriter(new OutputStreamWriter(clientSocket.getOutputStream()));
      log("[SortingServer]: Started communicating on socket #" + clientSocket.getPort() + ".");
    }

    @Override
    public void run() {
      while (!clientSocket.isClosed() && !finished && isRunning()) {
        String text;

        try {
          text = input.readLine();
          log("[SortingServer]: Received '" + text + "' from socket #" + this.clientSocket.getPort() + ".");
        } catch (SocketTimeoutException e) {
          continue;
        } catch (IOException e) {
          throw new RuntimeException(e);
        }

        String[] words = text.split(" ");

        if (words.length == 0 || words.length > 2) {
          sendUndefined();
        }

        if (words.length == 2) {
          sort(Integer.parseInt(words[0]), Integer.parseInt(words[1]));
        } else {
          switch (words[0]) {
            case "quit" -> quit();
            case "stopserver" -> stopServer();
            default -> sendUndefined();
          }
        }
      }

      log("[SortingServer]: Stopped communicating on socket #" + clientSocket.getPort() + ".");
    }

    /**
     * Line terminates and sends a response to the client
     * @param response the response to be sent
     */
    private void sendResponse(String response) {
      try {
        output.write(response);
        output.write('\n');
        output.flush();
        log("[SortingServer]: Sent '" + response + "' to socket #" + this.clientSocket.getPort() + ".");
      } catch (IOException e) {
        log("[SortingServer]: Failed to send '" + response + "' to socket #" + this.clientSocket.getPort() + ".");
        throw new RuntimeException(e);
      }
    }

    /** Sends an "undefined" response */
    private void sendUndefined() {
      sendResponse("undefined");
    }

    /**
     * Times the parallel sorting of an array of size `arraySize` filled with random integers using `nThreads` threads.
     * Sends the time taken to the client.
     * @param arraySize size of random array
     * @param nThreads number of threads used in the parallel sort
     */
    private void sort(int arraySize, int nThreads) {
      Integer[] array = new Integer[arraySize];
      Random random = new Random();

      for (int i = 0; i < arraySize; i += 1) {
        array[i] = random.nextInt();
      }

      long time = CodeTimer.timeCode(() -> Sorter.parallelMergeSort(array, nThreads));
      sendResponse(time/1_000_000 + " ms");
    }

    /** Marks this task as finished (on client quit). */
    private void quit() {
      this.finished = true;
    }

    /** Stops the server and marks this task as finished. */
    private void stopServer() {
      this.finished = true;
      setRunning(false);
    }
  }

  public synchronized boolean isRunning() {
    return this.running;
  }

  public synchronized void setRunning(boolean running) {
    this.running = running;
  }

  // Port 4242 test
  public static void main(String[] args)throws IOException {
    new SortingServer(4242);
  }
}
