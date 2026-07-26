module Arkham.Treachery.Cards.ElderMist (elderMist) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ElderMist = ElderMist TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elderMist :: TreacheryCard ElderMist
elderMist = treachery ElderMist Cards.elderMist

instance HasModifiersFor ElderMist where
  getModifiersFor (ElderMist a) =
    modifySelect
      a
      (InvestigatorAt $ locationWithTreachery a)
      [SkillModifier sType (-1) | sType <- [#willpower, #intellect, #combat, #agility]]

instance HasAbilities ElderMist where
  getAbilities (ElderMist a) =
    [ mkAbility a 1
        $ forced
        $ SkillTestResult #after (InvestigatorAt $ locationWithTreachery a) AnySkillTest
        $ SuccessResult (atLeast 2)
    ]

instance RunMessage ElderMist where
  runMessage msg t@(ElderMist attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ls <- select $ NearestLocationTo iid $ LocationWithoutTreachery (treacheryIs Cards.elderMist)
      chooseTargetM iid ls $ place attrs . AttachedToLocation
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> ElderMist <$> liftRunMessage msg attrs
