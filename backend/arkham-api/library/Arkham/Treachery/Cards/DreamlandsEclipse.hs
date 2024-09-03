module Arkham.Treachery.Cards.DreamlandsEclipse (dreamlandsEclipse, DreamlandsEclipse (..)) where

import Arkham.Ability
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.SkillTest (getSkillTestTarget, withSkillTest)
import Arkham.Matcher
import Arkham.Placement
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
      getSkillTestTarget >>= \case
        Nothing -> error "invalid window"
        Just target -> do
          withSkillTest \sid ->
            chooseOne
              iid
              [ Label "Take 1 horror" [Msg.assignHorror iid source 1]
              , Label
                  "Your location gets +2 shroud for this investigation"
                  [Msg.skillTestModifier sid source target (ShroudModifier 2)]
              ]
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> DreamlandsEclipse <$> liftRunMessage msg attrs
