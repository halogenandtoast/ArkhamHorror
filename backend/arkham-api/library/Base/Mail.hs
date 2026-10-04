{- | Sending plain text mail.

One place rather than one per sender, because every message this app sends is
the same shape: a from address nobody replies to, one recipient, a subject and
some lines.

'sendPlainTextEmail' never throws. Mail is something that happens alongside the
request, not the thing the request was for: a review that went through has gone
through whether or not the author's mail host was reachable, and failing the
handler would leave the admin looking at an error over a decision that was
already saved.
-}
module Base.Mail (sendPlainTextEmail) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.Mail.Mailtrap
import Relude
import Text.Email.Parser (unsafeEmailAddress)
import UnliftIO.Exception (tryAny)

{- | Mail one address. The category is what Mailtrap groups by, so it names the
kind of message rather than this particular one.
-}
sendPlainTextEmail
  :: MonadIO m
  => Token
  -> Text
  -- ^ to
  -> Text
  -- ^ subject
  -> Text
  -- ^ category
  -> [Text]
  -- ^ body, one line each
  -> m ()
sendPlainTextEmail token to subject category body =
  case parseEmailAddress (TE.encodeUtf8 to) of
    Left _ -> pure ()
    Right address ->
      void
        $ liftIO
        $ tryAny
        $ void
        $ sendEmail token
        $ Email
            { email_from =
                NamedEmailAddress (unsafeEmailAddress "noreply" "arkhamhorror.app") "No Reply"
            , email_to = [NamedEmailAddress address ""]
            , email_cc = []
            , email_bcc = []
            , email_attachments = []
            , email_custom = mempty
            , email_message =
                Right
                  $ Message
                    { message_subject = subject
                    , message_body = PlainTextBody (T.unlines body)
                    , message_category = category
                    }
            }
