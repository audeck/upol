#include <stdio.h>
#include <stdlib.h>

#define MAX_LENGTH 20  // Also used for salary; if your salary is longer than 20 figures, my bad :^)
#define BUFFER_SIZE (3 * MAX_LENGTH)



typedef struct {
    char jmeno[MAX_LENGTH];
    char prijmeni[MAX_LENGTH];
    int plat;
} zamestnanec;



/* Counts lines in a file (provided they're terminated with a newline character) */
int count_lines(char* file_name) {
    int line_count = 0;
    int cur_char;
    FILE* file;

    /* Open file & handle exception(s) */
    if ((file = fopen(file_name, "r")) == NULL) {
        fprintf(stderr, "[ERROR in count_lines()]: Failed to open file %s\n", file_name);
        return -1;
    }

    /* Count number of newline characters */
    while ((cur_char = fgetc(file)) != EOF) {
        if (cur_char == '\n') line_count += 1;
    }

    /* Close file & handle exception(s) */
    if (fclose(file) == EOF) {
        fprintf(stderr, "[ERROR in count_lines()]: Failed to close file %s\n", file_name);
        return -1;
    }

    return line_count;
}

/* Returns a zamestnanec struct created from parsing line */
zamestnanec employee_from_line(char* line) {
    zamestnanec employee = {"", ""};  // Flush strings(!)
    char salary[MAX_LENGTH];  // Type string(!)
    int i = 0;
    int j = 0;

    /* Copy name */
    while (line[i] != ';') {
        employee.jmeno[j] = line[i];
        i += 1;
        j += 1;
    }

    /* Skip ';' and reset j */
    i += 1;
    j = 0;

    /* Copy surname */
    while (line[i] != ';') {
        employee.prijmeni[j] = line[i];
        i += 1;
        j += 1;
    }

    /* Skip ';' and reset j */
    i += 1;
    j = 0;

    /* Copy salary string */
    while (line[i] != '\n') {
        salary[j] = line[i];
        i += 1;
        j += 1;
    }

    /* Convert string to int & assign to struct */
    employee.plat = atoi(salary);

    return employee;
}

zamestnanec* nacti_data(char* file_name, int* record_count) {
    /* Get line count (number of employees) & allocate employee array */
    if ((*record_count = count_lines(file_name)) == -1) return NULL;
    zamestnanec* employees = (zamestnanec*) malloc(*record_count * sizeof(zamestnanec));

    FILE* records;
    char record_buffer[BUFFER_SIZE];

    /* Open file & handle exception(s) */
    if ((records = fopen(file_name, "r")) == NULL) {
        fprintf(stderr, "[ERROR in nacti_data()]: Failed to open file %s\n", file_name);
        *record_count = 0;
        free(employees);
        return NULL;
    }

    /* Get employee data by line & insert data into array */
    for (int i = 0; i < *record_count; i += 1) {
        fgets(record_buffer, BUFFER_SIZE, (FILE*) records);
        employees[i] = employee_from_line(record_buffer);
    }

    /* Close file & handle exception(s) */
    if (fclose(records) == EOF) {
        fprintf(stderr, "[ERROR in nacti_data()]: Failed to close file %s\n", file_name);
        *record_count = 0;
        free(employees);
        return NULL;
    }

    return employees;
}

void vypis_pole(zamestnanec* pole, int pocet) {
    int i;
    for(i = 0; i < pocet; i++) 
        printf("%s %s %i\n",pole[i].jmeno , pole[i].prijmeni , pole[i].plat);
}



int main(void) {
    int employee_count;
    zamestnanec* employees = nacti_data("data_mensi.csv", &employee_count);
    if (employees != NULL) vypis_pole(employees, employee_count);
    free(employees);
    
    return 0;
}
