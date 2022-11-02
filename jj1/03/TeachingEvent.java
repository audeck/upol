package cz.upol.jj1;

import java.time.LocalDateTime;

/** This class implements functionality for both lectures and labs. */
public class TeachingEvent implements TimetableBlock, TimetableEvent {
  /** This TE's name */
  String name;
  /** This TE's type (either lecture or lab) */
  String eventType;
  /** This TE's beginning time */
  LocalDateTime beginTime;
  /** This TE's ending time */
  LocalDateTime endTime;
  /** This TE's place (room code) */
  String place;

  TeachingEvent(
      String name, String eventType, LocalDateTime beginTime, LocalDateTime endTime, String place) {
    this.name = name;
    this.eventType = eventType;
    this.beginTime = beginTime;
    this.endTime = endTime;
    this.place = place;
  }

  /** @return this TE's name */
  public String getTitle() {
    return this.name;
  }

  /** @return this TE's type */
  public String getType() {
    return this.eventType;
  }

  /** @return this TE's time range */
  public String getTime() {
    String begin = this.beginTime.getHour() + ":" + this.beginTime.getMinute();
    String end = this.endTime.getHour() + ":" + this.endTime.getMinute();

    return begin + "-" + end;
  }

  /** @return this TE's color */
  public String getColor() {
    return (this.eventType.equals("Přednáška")) ? "Red" : "Green";
  }

  /** @return this TE's room code */
  public String getPlace() {
    return this.place;
  }
}
