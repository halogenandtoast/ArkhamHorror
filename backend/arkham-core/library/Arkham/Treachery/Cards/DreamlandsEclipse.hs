module Arkham.Treachery.Cards.DreamlandsEclipse (
  dreamlandsEclipse,
  DreamlandsEclipse (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DreamlandsEclipse = DreamlandsEclipse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

dreamlandsEclipse :: TreacheryCard DreamlandsEclipse
dreamlandsEclipse = treachery DreamlandsEclipse Cards.dreamlandsEclipse

instance HasAbilities DreamlandsEclipse where
  getAbilities (DreamlandsEclipse a) =
    [ mkAbility a 1
        $ ForcedAbility
        $ InitiatedSkillTest #when You AnySkillType AnySkillTestValue #investigation
    , mkAbility a 2 $ ForcedAbility $ RoundEnds #when
    ]

instance RunMessage DreamlandsEclipse where
  runMessage msg t@(DreamlandsEclipse attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> do
      push $ PlaceTreachery (toId attrs) TreacheryNextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      let source = toAbilitySource attrs 1
      mTarget <- getSkillTestTarget
      case mTarget of
        Nothing -> error "invalid window"
        Just target -> do
          push
            $ chooseOne
              player
              [ Label "Take 1 horror" [assignHorror iid source 1]
              , Label
                  "Your location gets +2 shroud for this investigation"
                  [skillTestModifier source target (ShroudModifier 2)]
              ]
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ toDiscard (toAbilitySource attrs 2) attrs
      pure t
    _ -> DreamlandsEclipse <$> runMessage msg attrs
