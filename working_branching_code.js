function apple(x, y) {
  if (x < 1) {
    return 2;
  }

  if (x < 2) {
    if (y < 5) {
      return 4;
    }
    return 3;
  }

  if (x < 3) {
    return 4;
  }
  return 5;
}
