module Arkham.Helpers.CardOption (
  getCardOption,
  getCardOptionSet,
) where

import Arkham.Card (lookupCardDef)
import Arkham.Card.CardCode
import Arkham.Card.CardDef (cdOptions)
import Arkham.Card.CardOption
import Arkham.Card.Settings (lookupCardOption)
import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

{- | The value @iid@ has chosen for one of @cCode@'s declared options, falling
back to the default the card declares. 'Nothing' when the card declares no such
option.
-}
getCardOption :: (HasGame m, HasCardCode a) => InvestigatorId -> a -> Text -> m (Maybe OptionValue)
getCardOption iid (toCardCode -> cCode) k = case find ((== k) . cardOptionKey) declared of
  Nothing -> pure Nothing
  Just declaration -> do
    settings <- field InvestigatorSettings iid
    pure . Just $ fromMaybe (cardOptionDefault declaration) (lookupCardOption cCode k settings)
 where
  declared = maybe [] cdOptions (lookupCardDef cCode)

-- | Whether an option is on. An option the card doesn't declare is off.
getCardOptionSet :: (HasGame m, HasCardCode a) => InvestigatorId -> a -> Text -> m Bool
getCardOptionSet iid cCode k = maybe False optionValueToBool <$> getCardOption iid cCode k
