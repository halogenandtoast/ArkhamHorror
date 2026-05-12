module Arkham.Act.Cards.TheHeartOfTheHouse (theHeartOfTheHouse) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (EnemyAttacks)
import Arkham.EnemyLocation.Cards qualified as EnemyLocations
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTowards)
import Arkham.Scenarios.HemlockHouse.Helpers (locationIsUnsealed)
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Dormant, Room))

newtype TheHeartOfTheHouse = TheHeartOfTheHouse ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeartOfTheHouse :: ActCard TheHeartOfTheHouse
theHeartOfTheHouse = act (2, A) TheHeartOfTheHouse Cards.theHeartOfTheHouse Nothing

instance HasModifiersFor TheHeartOfTheHouse where
  getModifiersFor (TheHeartOfTheHouse a) =
    -- "Clues cannot be discovered from locations with no investigators."
    modifySelect a Anyone [CannotDiscoverCluesAt $ not_ $ LocationWithInvestigator Anyone]

instance HasAbilities TheHeartOfTheHouse where
  getAbilities (TheHeartOfTheHouse a) =
    [ -- Same seal action as Strange Infestation.
      restricted
        a
        1
        (exists $ YourLocation <> LocationWithTrait Dormant)
        $ actionAbilityWithCost (SameLocationGroupClueCost (PerPlayer 1) YourLocation)
    , -- "Forced - After an enemy-location attacks you: Move (one location at a
      -- time) to Shapeless Cellar." Enemy-locations carry the Room trait via
      -- their proxy so we match on that.
      mkAbility a 2
        $ forced
        $ EnemyAttacks #after You AnyEnemyAttack (EnemyWithTrait Room)
    , -- "Objective - When the Shapeless Cellar is in the victory display,
      -- advance."
      mkAbility a 3
        $ Objective
        $ forced
        $ AddedToVictory #after Nothing (cardIs EnemyLocations.shapelessCellar)
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
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      cellar <- selectOne $ locationIs EnemyLocations.shapelessCellar
      for_ cellar $ \cellarLid -> moveTowards (attrs.ability 2) iid cellarLid
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      advanceActDeck attrs
      pure a
    _ -> TheHeartOfTheHouse <$> liftRunMessage msg attrs
