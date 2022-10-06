package cz.upol.jj1;

class Person {
    int id;
    String firstName;
    String lastName;
    int age;
    String phone;
    Country country;

    Person(int id, String firstName, String lastName, int age, String phone, Country country) {
        this.id = id;
        this.firstName = firstName;
        this.lastName = lastName;
        this.age = age;
        this.phone = phone;
        this.country = country;
    }

    String getStatus() {
        if (age < 18) return "junior";
        else if (age >= 65) return "senior";
        else return "dospělý";
    }

    /* Returns the person's full phone number (including phone code) */
    String getPhone() {
        String phoneCode = (this.country != null) ? this.country.phoneCode : "";
        return phoneCode + this.phone;
    }

    /* Returns the person's full name ("firstName lastName") */
    String getFullName() {
        return this.firstName + " " + this.lastName;
    }

    /* Returns an attribute string: "attribute=attribute_value" */
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

    /* Returns a printable string: "Osoba{attribute1=value1, attribute2=value2, ...}" */
    private String getPrintableString(String[] attributes) {
        StringBuilder output = new StringBuilder("Osoba{");
        boolean isFirstAttribute = true;

        for (String attribute : attributes) {
            if (!isFirstAttribute) output.append(", ");
            output.append(getAttributeString(attribute));
            isFirstAttribute = false;
        }

        output.append("}");
        return output.toString();
    }

    /* Prints out the person's default printable string */
    void print() {
        String[] attributes = {"id", "jméno", "příjmení", "věk", "telefon", "status", "stát"};
        System.out.println(getPrintableString(attributes));
    }

    /* Returns the person's default INDEX printable string */
    private String getIndexString() {
        String[] attributes = {"id", "jméno", "příjmení", "telefon"};
        return getPrintableString(attributes);
    }
}
