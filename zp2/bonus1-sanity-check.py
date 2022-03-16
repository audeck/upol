import subprocess

if __name__ == "__main__":
    times = 1000  # Number of runs

    # Compile code w/ the c99 standard
    print("Compiling @ c99...")
    subprocess.call(["gcc", "-std=c99", "bonus1.c", "-o", "bonus1"])

    # Run the code 'times' times
    print(f"Running {times} times @ c99...")
    for i in range(0, times):
        output = subprocess.check_output("./bonus1")
        # Notify if stdout isn't... standard :^)
        if output.decode('UTF-8') != "Number of occurences: 2\n @ index 3\n @ index 11\n":
            print("Code failed!")

    # Compile code w/ the c11 standard
    print("Compiling @ c11...")
    subprocess.call(["gcc", "-std=c11", "bonus1.c", "-o", "bonus1"])

    # Run the code 'times' times again
    print(f"Running {times} times @ c11...")
    for i in range(0, times):
        output = subprocess.check_output("./bonus1")
        # Notify if stdout isn't... standard :^)
        if output.decode('UTF-8') != "Number of occurences: 2\n @ index 3\n @ index 11\n":
            print("Code failed!")

    print("Done!")
