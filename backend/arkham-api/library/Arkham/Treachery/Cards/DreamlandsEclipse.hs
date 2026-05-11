module Arkham.Treachery.Cards.DreamlandsEclipse (dreamlandsEclipse, DreamlandsEclipse (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (getSkillTestTargetedLocation, withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenarios.TheSearchForKadath.Helpers (scenarioI18n)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DreamlandsEclipse = DreamlandsEclipse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamlandsEclipse :: TreacheryCard DreamlandsEclipse
dreamlandsEclipse = treachery DreamlandsEclipse Cards.dreamlandsEclipse

instance HasAbilities DreamlandsEclipse where
  getAbilities (DreamlandsEclipse a) =
    [ mkAbility a 1 $ forced $ InitiatedSkillTest #when You #any #any #investigation
    , mkAbility a 2 $ forced $ RoundEnds #when
    ]

instance RunMessage DreamlandsEclipse where
  runMessage msg t@(DreamlandsEclipse attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      getSkillTestTargetedLocation >>= \case
        Nothing -> error "invalid window"
        Just lid -> do
          withSkillTest \sid -> chooseOneM iid do
            withI18n $ countVar 1 $ labeledI "takeHorror" $ assignHorror iid source 1
            scenarioI18n $ scope "dreamlandsEclipse" $ labeled' "shroudPlus2" do
              skillTestModifier sid source lid (ShroudModifier 2)
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> DreamlandsEclipse <$> liftRunMessage msg attrs
