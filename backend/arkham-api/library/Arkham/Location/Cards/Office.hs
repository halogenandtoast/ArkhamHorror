module Arkham.Location.Cards.Office (office) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (SilverTwilight))

newtype Office = Office LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

office :: LocationCard Office
office = location Office Cards.office 4 (PerPlayer 1)

instance HasModifiersFor Office where
  getModifiersFor (Office a) = modifySelect a (enemyAt a) [RemoveKeyword Keyword.Aloof]

instance HasAbilities Office where
  getAbilities (Office a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( DuringSkillTest (WhileInvestigating (be a))
            <> exists (EnemyWithTrait SilverTwilight <> EnemyWithoutModifier CannotPlaceDoomOnThis)
        )
      $ forced
      $ RevealChaosToken #after You
      $ mapOneOf ChaosTokenFaceIs [Skull, Cultist, Tablet, ElderThing, AutoFail]

instance RunMessage Office where
  runMessage msg l@(Office attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <-
        select
          $ NearestEnemyToLocationFallback
            attrs.id
            (EnemyWithTrait SilverTwilight <> EnemyWithoutModifier CannotPlaceDoomOnThis)
      chooseOrRunOneM iid $ targets enemies $ placeDoomOn (attrs.ability 1) 1
      pure l
    _ -> Office <$> liftRunMessage msg attrs
