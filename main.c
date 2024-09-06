/* Function declarations */
int add(int a, int b);
void print_array(int arr[], int size);

/* Enum definition */
enum Color { RED, GREEN, BLUE };

/* Struct definition */
struct Point {
    int x;
    int y;
};

// This is valid according to the grammar, but not according to the standart.
// In theory this would be caught later on.
int int c = 10;


int main() {
    /* Variable declarations */
    // Note: These need to be at the top of the function.
    // Since translation_unit -> external_declaration -> function_definition -> [...] compount_statement ->
    // { declaration_list statement_list }
    // And the statement list doesn't allow top level declarations. Instead it requires further declarations
    // to be inside a "{}" compound statement again.
    // This will be illustrated further down.
    int x = 5;
    int y = 3;
    int octalNumber = 0754; // Octal representation
    int b = 0x1A3F; // Hexadecimal (hexadecimal representation of 6719)
    float f = 3.14f;       // Decimal notation
    double d = 1.23e4;     // Scientific notation (1.23 * 10^4)
    long double ld = 1.234567890123456789L; // Long double with more precision
    int result;
    int arr[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    enum Color favorite_color = GREEN; /* Enum usage */
    struct Point p1; /* Struct variable declaration */
    
    /* Struct member initialization */
    p1.x = 10;
    p1.y = 20;

    /* If-else statement */
    if (x > y) {
        result = add(x, y);
    } else {
        result = x - y;
    }
    
    /* Switch statement using enum */
    switch (favorite_color) {
        case RED:
            x = 1;
            break;
        case GREEN:
            x = 2;
            break;
        case BLUE:
            x = 3;
            break;
        default:
            x = 0;
    }

    /* Using a function call */
    result = add(x, result);
    
    /* For loop */
    for (x = 0; x < 10; x++) {
        arr[x] = arr[x] * 2;
    }

    print_array(arr, 10);
    
    /* While loop */
    x = 0;
    while (x < 5) {
        x++;
    }
    

    // This isn't allowed:
    // int c = 10;
    // Since declarations which are not part of compound statements
    // are only allowed on the top level.

    // Funnily enough, this is:
    {
        int a = 10; 
    }

    // MORE FUN AND VALID C CODE
    if (u = 6, "a") {
        e ++;
    }

    return 0;
}

/* Function definitions */
int add(int a, int b) {
    return a + b;
}

void print_array(int arr[], int size) {
    int i;
    for (i = 0; i < size; i++) {
        /* Function call and array access */
        arr[i] = arr[i] * 2;
    }
}
