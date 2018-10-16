typedef int Sudoku[9][9];

void afficher_sudoku(Sudoku S);

int impossible(Sudoku S);

void initialiser_sudoku(Sudoku S, FILE* fichier);

int verification_sudoku(Sudoku S);

void trouver_vide(Sudoku S, int* x, int* y);

void trouver_jouables(Sudoku S, int ligne, int colonne, int jouables[9]);

int resoudre(Sudoku S, unsigned int* compteur);
