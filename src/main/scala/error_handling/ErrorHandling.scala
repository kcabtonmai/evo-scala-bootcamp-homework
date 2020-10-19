package error_handling

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxTuple4Semigroupal, catsSyntaxValidatedIdBinCompat0}
import cats.syntax.all._
import scala.util.control.NonFatal


object Homework {

  case class PaymentCard(nameOnCard: String, cardNumber: Long, expire: String, cvv: Int)

  sealed trait ValidationError

  object ValidationError {

    final case object NameLengthInvalid extends ValidationError {
      override def toString: String = "Name on card must contain full name"
    }

    final case object NameCharsInvalid extends ValidationError {
      override def toString: String = "Name should not contain special characters"
    }

    final case object CardNumberInvalid extends ValidationError {
      override def toString: String = "Card number must be exactly 16 digits"
    }

    final case object CardTypeUnsupported extends ValidationError {
      override def toString: String = "Only mastercard is supported"
    }

    final case object CardExpiryFormat extends ValidationError {
      override def toString: String = "Expiry date must be in format MM/YY"
    }

    final case object CardExpired extends ValidationError {
      override def toString: String = "Payment card is expired"
    }

    final case object CVVInvalid extends ValidationError {
      override def toString: String = "CVV number must be exactly 3 digits"
    }

  }

  object PaymentCardValidator {

    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateName(name: String): AllErrorsOr[String] = {
      def validateNameLength: AllErrorsOr[String] =
        if (name.length >= 3 && name.length <= 30) name.validNec
        else NameLengthInvalid.invalidNec

      def validateNameContents: AllErrorsOr[String] =
        if (name.matches("^[a-zA-Z0-9 ]+$")) name.validNec
        else NameCharsInvalid.invalidNec

      validateNameLength.productR(validateNameContents)
    }

    private def validateNumber(num: String): AllErrorsOr[Long] = {

      def validateNumberInput: AllErrorsOr[Long] =
        num.toLongOption match {
          case None => CardNumberInvalid.invalidNec
          case Some(a) => a.toLong.validNec
        }

      def validateNumberLength(num: Long): AllErrorsOr[Long] =
        if (num.toString.length == 16) num.toLong.validNec
        else CardNumberInvalid.invalidNec

      def validateCardType(num: Long): AllErrorsOr[Long] =
        if (num.toString.take(2).toInt >= 51 && num.toString.take(2).toInt <= 55) num.toLong.validNec
        else CardTypeUnsupported.invalidNec

      validateNumberInput andThen validateNumberLength andThen validateCardType
    }

    private def validateExpiry(expire: String): AllErrorsOr[String] = {

      def validateExpiry: AllErrorsOr[String] = {
        try {
          val split = expire.split('/')
          val Month = java.time.Month.of(split(0).toInt)
          val Year = java.time.Year.parse(split(1), java.time.format.DateTimeFormatter.ofPattern("yy"))

          if (java.time.Year.now.getValue < Year.getValue)
            expire.validNec
          else if (java.time.Year.now.getValue == Year.getValue && java.time.LocalDate.now().getMonthValue <= Month.getValue)
            expire.validNec
          else CardExpired.invalidNec
        } catch {
          case NonFatal(t) => CardExpiryFormat.invalidNec
        }
      }

      validateExpiry
    }

    private def validateCVV(cvv: String): AllErrorsOr[Int] = {

      def validateCVVInput: AllErrorsOr[Int] =
        cvv.toLongOption match {
          case None => CVVInvalid.invalidNec
          case Some(a) => a.toInt.validNec
        }

      def validateCVVData(cvv: Int): AllErrorsOr[Int] = {
        if (cvv.toString.length == 3) cvv.validNec
        else CVVInvalid.invalidNec
      }

      validateCVVInput andThen validateCVVData
    }

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = {
      (validateName(name),
        validateNumber(number),
        validateExpiry(expirationDate),
        validateCVV(securityCode)
        ).mapN(PaymentCard)
    }
  }
}

