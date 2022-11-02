package cz.upol.jj1;

/** This class implements some functionality of a person living in a country. */
class Person {
  /** The person's ID */
  int id;
  /** The person's first name */
  String firstName;
  /** The person's last name */
  String lastName;
  /** The person's age */
  int age;
  /** The person's phone number */
  String phone;
  /** The person's country */
  Country country;

  Person(int id, String firstName, String lastName, int age, String phone, Country country) {
    this.id = id;
    this.firstName = firstName;
    this.lastName = lastName;
    this.age = age;
    this.phone = phone;
    this.country = country;
  }

  /**
   * @return the person's status
   */
  public String getStatus() {
    if (this.age < 18) {
      return "junior";
    } else if (this.age >= 65) {
      return "senior";
    } else {
      return "dospělý";
    }
  }

  /**
   * @return the person's full phone number (including phone code)
   */
  String getPhone() {
    String phoneCode = (this.country != null) ? this.country.phoneCode : "";
    return phoneCode + this.phone;
  }

  /**
   * @return the person's full name ("firstName lastName")
   */
  String getFullName() {
    return this.firstName + " " + this.lastName;
  }

  /**
   * @return an attribute string: "attribute=attribute_value"
   */
  private String getAttributeString(String attribute) {
    return switch (attribute) {
      case "id" -> "id=" + this.id;
      case "jméno" -> "jméno=" + this.firstName;
      case "příjmení" -> "příjmení=" + this.lastName;
      case "věk" -> "věk=" + this.age;
      case "telefon" -> "telefon=" + this.getPhone();
      case "status" -> "status=" + this.getStatus();
      case "stát" -> "stát=" + ((this.country != null) ? this.country.name : "neznámý");
      default -> "UNKNOWN ATTRIBUTE";
    };
  }

  /**
   * @return a printable string: "Osoba{attribute1=value1, attribute2=value2, ...}"
   */
  private String getPrintableString(String[] attributes) {
    StringBuilder output = new StringBuilder("Osoba{");
    boolean isFirstAttribute = true;

    for (String attribute : attributes) {
      // Separate all but the first attribute using ", "
      if (!isFirstAttribute) {
        output.append(", ");
      }
      output.append(getAttributeString(attribute));
      isFirstAttribute = false;
    }

    output.append("}");
    return output.toString();
  }

  /** Prints out the person's default printable string. */
  void print() {
    String[] attributes = {"id", "jméno", "příjmení", "věk", "telefon", "status", "stát"};
    System.out.println(getPrintableString(attributes));
  }

  /**
   * @return the person's default INDEX printable string
   */
  String getIndexString() {
    String[] attributes = {"id", "jméno", "příjmení", "telefon"};
    return getPrintableString(attributes);
  }
}
