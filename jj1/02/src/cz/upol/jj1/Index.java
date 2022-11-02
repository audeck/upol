package cz.upol.jj1;

/** This class implements an index, which contains arrays of people and countries */
class Index {
  /** Array of people in this index */
  Person[] persons;
  /** Array of countries in this index */
  Country[] countries;

  Index(Person[] persons, Country[] countries) {
    this.persons = persons;
    this.countries = countries;
  }

  /** Returns the number of people in living in a given country. */
  int count(String countryName) {
    int count = 0;

    for (Person person : this.persons) {
      if (person.country != null && person.country.name.equals(countryName)) {
        count += 1;
      }
    }

    return count;
  }

  /** Returns the number of people of given first name and last name. */
  int count(String firstName, String lastName) {
    int count = 0;

    for (Person person : this.persons) {
      if (person.firstName.equals(firstName) && person.lastName.equals(lastName)) {
        count += 1;
      }
    }

    return count;
  }

  /**
   * Returns the person of a given id.
   *
   * @return {@code null} if not found, otherwise the found person
   */
  Person search(int id) {
    for (Person person : this.persons) {
      if (person.id == id) {
        return person;
      }
    }
    return null;
  }

  /**
   * @return an array of people living in a given country
   */
  Person[] search(String countryName) {
    Person[] output = new Person[count(countryName)];
    int outputIndex = 0;

    for (Person person : this.persons) {
      if (person.country != null && person.country.name.equals(countryName)) {
        output[outputIndex] = person;
        outputIndex += 1;
      }
    }

    return output;
  }

  /**
   * @return an array of people of given first name and last name
   */
  Person[] search(String firstName, String lastName) {
    Person[] output = new Person[count(firstName, lastName)];
    int outputIndex = 0;

    for (Person person : this.persons) {
      if (person.firstName.equals(firstName) && person.lastName.equals(lastName)) {
        output[outputIndex] = person;
        outputIndex += 1;
      }
    }

    return output;
  }

  /**
   * Sets a person's country to a country given by phone code.
   *
   * @param person person whose country to set
   * @param phoneCode phone code to set country by
   * @return {@code true} if country is set successfully, otherwise {@code false}
   */
  boolean setCountryByPhoneCode(Person person, String phoneCode) {
    for (Country country : this.countries) {
      if (country.phoneCode.equals(phoneCode)) {
        person.country = country;
        return true;
      }
    }

    return false;
  }

  /** Prints out the default index string */
  void print() {
    StringBuilder output = new StringBuilder("Rejstřík{ ");

    for (Person person : this.persons) {
      output.append(person.getIndexString());
      output.append(" ");
    }

    output.append(" }");
    System.out.println(output.toString());
  }
}
