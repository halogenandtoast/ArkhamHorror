module Arkham.Act.Cards.StrangeInfestation (strangeInfestation) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Scenarios.HemlockHouse.Helpers (locationIsUnsealed)
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Dormant))

newtype StrangeInfestation = StrangeInfestation ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeInfestation :: ActCard StrangeInfestation
strangeInfestation = act (1, A) StrangeInfestation Cards.strangeInfestation Nothing

instance HasModifiersFor StrangeInfestation where
  getModifiersFor (StrangeInfestation a) = do
    -- "Clues cannot be discovered from locations with no investigators."
    modifySelect a Anyone [CannotDiscoverCluesAt $ not_ $ LocationWithInvestigator Anyone]

instance HasAbilities StrangeInfestation where
  getAbilities (StrangeInfestation a) =
    [ -- "[action] If your location is unsealed and [[Dormant]], investigators
      -- at that location spend 1 [per_investigator] clues, as a group: Place 1
      -- resource on it, as a seal."
      restricted
        a
        1
        (exists $ YourLocation <> LocationWithTrait Dormant)
        actionAbility
    , -- "Objective - At the end of the round, if a total of 7 locations are
      -- sealed and/or in the victory display, advance."
      mkAbility a 2
        $ Objective
        $ forced (RoundEnds #when)
    ]

instance RunMessage StrangeInfestation where
  runMessage msg a@(StrangeInfestation attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      mlid <- getMaybeLocation iid
      case mlid of
        Nothing -> pure ()
        Just lid -> do
          unsealed <- locationIsUnsealed lid
          when unsealed $
            -- TODO: enforce "investigators at that location spend 1 [per_investigator]
            -- clues, as a group" via a proper group-cost wrapper.
            push $ PlaceTokens (toSource (attrs.ability 1)) (toTarget lid) Resource 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      sealed <- length <$> select (LocationWithToken Resource)
      -- TODO: also count locations in the victory display once EnemyLocationDefeated
      -- adds them via AddToVictory.
      when (sealed >= 7) $ advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> StrangeInfestation <$> liftRunMessage msg attrs
