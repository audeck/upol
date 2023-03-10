package cz.upol.jj2.client;

import java.io.*;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;
import java.util.Scanner;

public class SortingClient implements AutoCloseable {
  private final Socket clientSocket;
  private final BufferedReader input;
  private final BufferedWriter output;

  public SortingClient(InetAddress ip, int port) throws IOException {
    this.clientSocket = new Socket(ip, port);
    this.input = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
    this.output = new BufferedWriter(new OutputStreamWriter(clientSocket.getOutputStream()));
  }

  /**
   * Sends a string to the server along with a line termination character ('\n').
   * @param request the string to be sent
   */
  public void sendRequest(String request) throws IOException {
    output.write(request);
    output.write('\n');
    output.flush();
  }

  /**
   * Reads a single line of text from the server.
   * @return the line of text not containing any line termination characters
   * @see java.io.BufferedReader#readLine()
   */
  public String readResponse() throws IOException {
    return input.readLine();
  }

  @Override
  public void close() {
    try {
      this.clientSocket.close();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  /** Simple CLI SortingClient implementation */
  public static void main(String[] args) throws IOException {
    InetAddress ip = InetAddress.getLocalHost();
    try (SortingClient client = new SortingClient(ip, 4242)) {
      Scanner scanner = new Scanner(System.in);
      System.out.println("[SortingClient]: Listening for requests.");

      while (true) {
        String request = scanner.nextLine();
        try {
          client.sendRequest(request);
        } catch (SocketException e) {
          break;
        }

        if (request.equalsIgnoreCase("quit") || request.equalsIgnoreCase("stopserver")) {
          break;
        }

        System.out.println(client.readResponse());
      }
    }

    System.out.println("[SortingClient]: Connection closed.");
  }
}
