What is the point of the : after declaring the arguements of the function? 

template <typename T>
matrix<T>::matrix(const size_t &input_rows, const size_t &input_cols, const initializer_list<T> &input_elements)
    : matrix(input_rows, input_cols, vector<T>{input_elements}) {}

or

template <typename T>
matrix<T>::matrix(const matrix<T> &m)
    : rows(m.rows), cols(m.cols)
{
    elements = new T[rows * cols];
    for (size_t i{0}; i < rows * cols; i++)
        elements[i] = m.elements[i];
}

How can this be applied to classes/ Did I apply this to classes correctly?

class solve_OLS
{
public:
    solve_OLS(matrix<double> &X, matrix<double> &Y)
        : R(X.get_rows(), Y.get_cols())
    {
    }
    matrix<double> solve();

    class x_and_y_not_same_size
    {
    };

    class matrix_is_empty
    {
    };

private:
    matrix<double> R;
};

matrix<double> solve_OLS::solve()
{
    R(1, 1) = 0;
    return R;
}

I want to show that I create my own template as well. Should I do this for this class type?

Easy part will be implementing shooting algorithm/working with the actual lasso regression part.
Difficulty is with making it smooth and seamless and have proper classes and member functions that work together.
Consideration for releading memory at the end of the function to only keep what is necessary.
