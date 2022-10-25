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
    for (Animal animal : this.animals) {
      AnimalSpecies species = animal.getSpecies();

      String output = animal.getName()
          + " je "
          + species.getName(animal.isMale())
          + " a dělá "
          + species.getAnimalSound()
          + ".";

      System.out.println(output);
    }
  }
}
