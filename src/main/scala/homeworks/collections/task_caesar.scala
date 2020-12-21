package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {

  val alphabetLength: Int = 'Z'.toChar - 'A'.toChar + 1
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
  def encrypt(word: String, offset: Int): String =
    word.map { ch =>
      val normOffset = offset % alphabetLength
      val chCode = 'A'.toChar + (ch.toByte - 'A'.toByte + normOffset) % alphabetLength
      chCode.toChar
    }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String =
    cipher.map { ch =>
      val normOffset = offset % alphabetLength
      val chCode = 'A'.toChar + (ch.toByte - 'A'.toByte + alphabetLength - normOffset) % alphabetLength
      chCode.toChar
    }

}
