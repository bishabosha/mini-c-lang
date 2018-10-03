#include <string>
#include <variant>
#include <iostream>

using int_char = std::variant<int, char>;

struct int_char_to_string {
  std::string operator()(int value) { return std::to_string(value); }

  std::string operator()(char value) { return std::string(1, value); }
};

int main() {
  int_char a(1);
  int_char b('a');
  
  std::string x("xyz");

  std::cout << "x: " << x << std::endl;
  auto const y = std::move(x);

  std::cout << "x: " << x << std::endl;
  std::cout << "y: " << y << std::endl;

  auto f = int_char_to_string();
  auto const a_result = std::visit(f, a);
  auto const b_result = std::visit(f, b);

  std::cout << "a: " << a_result << std::endl;
  std::cout << "b: " << b_result << std::endl;

  return 0;
}