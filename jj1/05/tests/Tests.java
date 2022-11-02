import static cz.upol.jj1.Main.formatStr;
import static org.junit.jupiter.api.Assertions.assertEquals;

import cz.upol.jj1.Animal;
import cz.upol.jj1.AnimalFarm;
import cz.upol.jj1.AnimalSpecies;
import cz.upol.jj1.Gender;
import org.junit.jupiter.api.Test;

public class Tests {
  String format;
  String result;
  AnimalFarm farm;
  Animal animal1;
  Animal animal2;
  Animal animal3;
  Animal animal4;

  @Test
  public void formatTest1() {
    format = "A: %0; B: %001; C: %0";
    result = "A: 1; B: 1.6; C: 1";
    assertEquals(formatStr(format, 1, 1.6), result);
  }

  @Test
  public void formatTest2() {
    format = "A: %0; B: %2; C: %1";
    result = "A: Hello!; B: %2; C: a";
    assertEquals(formatStr(format, "Hello!", 'a'), result);
  }

  @Test void animalTest1() {
    animal1 = new Animal("Alík", AnimalSpecies.DOG, Gender.MALE);
    assertEquals(animal1.getSpeciesName(), "pes");
    assertEquals(animal1.getSound(), "haf-haf");
  }

  @Test
  public void animalTest2() {
    animal2 = new Animal("Bobík", AnimalSpecies.DUCK, Gender.FEMALE);
    assertEquals(animal2.getSpeciesName(), "kačena");
    assertEquals(animal2.getSound(), "ga-ga");
  }

  @Test
  public void farmTest() {
    animal1 = new Animal("Alík", AnimalSpecies.DOG, Gender.MALE);
    animal2 = new Animal("Bobík", AnimalSpecies.DUCK, Gender.FEMALE);
    animal3 = new Animal("Chubaka", AnimalSpecies.DOG, Gender.FEMALE);
    animal4 = new Animal("Donald", AnimalSpecies.DUCK, Gender.MALE);

    farm = new AnimalFarm(animal1, animal2, animal3);
    farm.add(animal4);

    assertEquals(
        farm.list(),
        """
            Alík je pes a dělá haf-haf.
            Bobík je kačena a dělá ga-ga.
            Chubaka je fena a dělá haf-haf.
            Donald je kačer a dělá ga-ga.
            """);
  }
}