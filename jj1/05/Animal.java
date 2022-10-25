package cz.upol.jj1;

public class Animal {

  private String name;
  private AnimalSpecies species;
  private boolean isMale;

  public Animal(String name, AnimalSpecies species, boolean isMale) {
    this.name = name;
    this.species = species;
    this.isMale = isMale;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public AnimalSpecies getSpecies() {
    return species;
  }

  public void setSpecies(AnimalSpecies species) {
    this.species = species;
  }

  public boolean isMale() {
    return isMale;
  }

  public void setMale(boolean male) {
    isMale = male;
  }
}
