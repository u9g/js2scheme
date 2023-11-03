function apple(x) {
  let y = x + 2;
  if (x < 1) {
    let j = 10 + x;
    return j;
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
