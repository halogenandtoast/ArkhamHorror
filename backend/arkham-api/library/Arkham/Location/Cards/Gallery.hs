module Arkham.Location.Cards.Gallery (
  gallery,
  Gallery (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype Gallery = Gallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gallery :: LocationCard Gallery
gallery = location Gallery Cards.gallery 1 (Static 0)

instance HasAbilities Gallery where
  getAbilities (Gallery attrs) =
    withBaseAbilities
      attrs
      [ skillTestAbility $ restrictedAbility attrs 1 Here $ ForcedAbility $ TurnEnds Timing.After You
      | locationRevealed attrs
      ]

instance RunMessage Gallery where
  runMessage msg l@(Gallery attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid SkillWillpower (Fixed 2)
      pure l
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ | isAbilitySource attrs 1 source -> do
      clueCount <- field InvestigatorClues iid
      when (clueCount > 0) $ do
        pushAll [InvestigatorSpendClues iid 1, PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 1]
      pure l
    _ -> Gallery <$> runMessage msg attrs
