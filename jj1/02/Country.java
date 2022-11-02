package cz.upol.jj1;

/** This class implements a simple country */
class Country {
  /** The name of this country */
  String name;
  /** The phone code of this country */
  String phoneCode;

  Country(String name, String phoneCode) {
    this.name = name;
    this.phoneCode = phoneCode;
  }

  /** Prints out this country's special string */
  void print() {
    String output = "Stát{jméno=" + this.name + ", předvolba=" + this.phoneCode + "}";
    System.out.println(output);
  }
}
