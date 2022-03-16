import subprocess


def runCode(times, standard):
    # Compile code using the 'standard' standard
    print(f"Compiling @ {standard}...")

    try:
        subprocess.call(["gcc", f"-std={standard}", "bonus1.c", "-o", "bonus1"])

    except subprocess.SubprocessError or subprocess.CalledProcessError:  # Doesn't catch gcc errors and warnings :(
        print(f"Compilation @ {standard} failed!")

    finally:
        # Run the code 'times' times
        print(f"Running {times} times @ {standard}...")
        for i in range(0, times):
            output = subprocess.check_output("./bonus1")
            # Notify if stdout isn't... standard :^)
            if output.decode('UTF-8') != "Number of occurences: 2\n @ index 3\n @ index 11\n":
                print("Code failed!")


if __name__ == "__main__":
    # runCode(1000, "c89")
    runCode(1000, "c99")
    runCode(1000, "c11")

    print("Done!")
