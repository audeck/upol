package cz.upol.jj1;

import java.time.LocalDateTime;

/** This class implements functionality for exams. */
public class Exam implements TimetableBlock, TimetableEvent {
  /** This exam's name */
  String name;
  /** This exam's starting time */
  LocalDateTime time;
  /** This exam's place (room code) */
  String place;
  /** This exam's current attendance */
  int currentAttendance;
  /** This exam's maximum attendance */
  int maxAttendance;

  Exam(String name, LocalDateTime time, String place, int currentAttendance, int maxAttendance) {
    this.name = name;
    this.time = time;
    this.place = place;
    this.currentAttendance = currentAttendance;
    this.maxAttendance = maxAttendance;
  }

  /** @return this exam's name */
  public String getTitle() {
    return this.name;
  }

  /** @return this exam's type */
  public String getType() {
    return "Zkouška";
  }

  /** @return this exam's starting time */
  public String getTime() {
    return this.time.getHour() + ":" + this.time.getMinute();
  }

  /** @return this exam's color */
  public String getColor() {
    return "Orange";  // Along the lines of Colors.ORANGE in the future (with enums)
  }

  /** @return this exam's room code */
  public String getPlace() {
    return this.place;
  }

  /** @return this exam's attendance */
  public String getAttendance() {
    return this.currentAttendance + "/" + this.maxAttendance;
  }
}
