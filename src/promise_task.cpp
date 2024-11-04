#include <later_api.h>

#include <cpp11.hpp>

class PromiseTask : public later::BackgroundTask {
 public:
  PromiseTask(cpp11::function resolve, cpp11::function reject)
      : resolve(resolve), reject(reject) {}

 protected:
  virtual void execute() = 0;
  virtual cpp11::sexp get_result() = 0;

  void complete() {
    cpp11::sexp result = get_result();
    resolve(result);
  }

 private:
  cpp11::function resolve;
  cpp11::function reject;
};

long fib(long x) {
  if (x <= 2) {
    return 1;
  }
  return fib(x - 1) + fib(x - 2);
}

class FibonacciTask : public PromiseTask {
 public:
  FibonacciTask(cpp11::function resolve, cpp11::function reject, double x)
      : PromiseTask(resolve, reject), x(x) {}

  void execute() { result = fib((long)x); }

  cpp11::sexp get_result() {
    cpp11::writable::doubles res(1);
    res[0] = (double)result;
    return res;
  }

 private:
  double x;
  long result;
};

[[cpp11::register]] void asyncFib(cpp11::function resolve,
  cpp11::function reject, double x) {
  FibonacciTask* fib = new FibonacciTask(resolve, reject, x);
  fib->begin();
}

/*** R
library(promises)
library(later)
library(cpp11)

promise(function(resolve, reject) {
  cpp11::package("your_package_name")$asyncFib(resolve, reject, 45)
}) %...>% print()
*/
