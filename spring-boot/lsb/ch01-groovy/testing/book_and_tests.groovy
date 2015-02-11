// spring test book_and_tests.groovy

class Book {
  String author
  String title
}

class BookTests {
  @Test
  void testBooks() {
    Book book = new Book(author: "Tom Clancy", title: "Threat Vector")
    assertEquals("Tom Clancy", book.author)
  } 
}