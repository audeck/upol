package cz.upol.jj1;

import java.time.Duration;
import java.time.LocalDateTime;

/** This class implements all the functionality of a "free time block" in timetable. */
public class FreeTime implements TimetableBlock {
  /** Type of this event (should be enum) */
  String type = "Volno";
  /** Time of this event's beginning */
  LocalDateTime beginTime;
  /** Time of this event's ending */
  LocalDateTime endTime;

  public FreeTime(LocalDateTime beginTime, LocalDateTime endTime) {
    this.beginTime = beginTime;
    this.endTime = endTime;
  }

  /** @return this free time's title (it's duration) */
  public String getTitle() {
    Duration duration = Duration.between(this.beginTime, this.endTime);
    String hourText = (duration.toHours() > 1) ? "hodiny" : "hodina";

    return duration.toHours() + hourText + (duration.toMinutes() % 60) + "minut";
  }

  /**
   * Returns this free time's type, which is its subtitle.
   *
   * @return this free time type
   */
  public String getType() {
    return this.type;
  }

  /** @return this free time's time range */
  public String getTime() {
    String begin = this.beginTime.getHour() + ":" + this.beginTime.getMinute();
    String end = this.endTime.getHour() + ":" + this.endTime.getMinute();

    return begin + "-" + end;
  }

  /** @return this free time's color */
  public String getColor() {
    return "Clear";
  }
}
