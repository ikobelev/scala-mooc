package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String = doCrypt(word, offset, false)

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = doCrypt(cipher, offset, true)

  def doCrypt(word: String, offset: Int, reverseDirection: Boolean): String = {
    val alphabetLength: Int = ('A' to 'Z').length
    val normOffset =
      if (reverseDirection) alphabetLength - offset % alphabetLength
      else offset % alphabetLength
    word.map { ch =>
      val chCode = 'A' + (ch - 'A' + normOffset) % alphabetLength
      chCode.toChar
    }
  }
}
