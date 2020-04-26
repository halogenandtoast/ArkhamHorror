CREATE TABLE arkham_product_sets (
  id SERIAL PRIMARY KEY,
  type TEXT NOT NULL,
  title TEXT NOT NULL,
  UNIQUE (title)
);
