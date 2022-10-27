package cz.upol.jj1;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class AnimalFarm {

  private List<Animal> animals;

  public AnimalFarm(Animal... animals) {
    this.animals = new ArrayList<Animal>();
    this.animals.addAll(Arrays.asList(animals));
  }

  public void add(Animal animal) {
    this.animals.add(animal);
  }

  public void list() {
    String output;

    for (Animal animal : this.animals) {
      output = animal.getName() + " je " + animal.getSpeciesName() + " a dělá " + animal.getSound()
          + ".";
      System.out.println(output);
    }
  }
}
