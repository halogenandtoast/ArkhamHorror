module Arkham.Act.Cards.TheHeartOfTheHouse (theHeartOfTheHouse) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Matcher
import Arkham.Scenarios.HemlockHouse.Helpers (locationIsUnsealed)
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Dormant))

newtype TheHeartOfTheHouse = TheHeartOfTheHouse ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeartOfTheHouse :: ActCard TheHeartOfTheHouse
theHeartOfTheHouse = act (2, A) TheHeartOfTheHouse Cards.theHeartOfTheHouse Nothing

instance HasModifiersFor TheHeartOfTheHouse where
  getModifiersFor (TheHeartOfTheHouse a) = do
    -- "Clues cannot be discovered from locations with no investigators."
    modifySelect a Anyone [CannotDiscoverCluesAt $ not_ $ LocationWithInvestigator Anyone]

instance HasAbilities TheHeartOfTheHouse where
  getAbilities (TheHeartOfTheHouse a) =
    [ -- Same seal action as Strange Infestation.
      restricted
        a
        1
        (exists $ YourLocation <> LocationWithTrait Dormant)
        actionAbility
    ]

instance RunMessage TheHeartOfTheHouse where
  runMessage msg a@(TheHeartOfTheHouse attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      mlid <- getMaybeLocation iid
      case mlid of
        Nothing -> pure ()
        Just lid -> do
          unsealed <- locationIsUnsealed lid
          when unsealed
            $ push
            $ PlaceTokens (toSource (attrs.ability 1)) (toTarget lid) Resource 1
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      _ <- allInvestigators
      advanceActDeck attrs
      pure a
    _ -> TheHeartOfTheHouse <$> liftRunMessage msg attrs
