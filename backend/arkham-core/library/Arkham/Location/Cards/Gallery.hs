module Arkham.Location.Cards.Gallery
  ( gallery
  , Gallery(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype Gallery = Gallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gallery :: LocationCard Gallery
gallery = location Gallery Cards.gallery 1 (Static 0)

instance HasAbilities Gallery where
  getAbilities (Gallery attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here $ ForcedAbility $ TurnEnds Timing.After You
    | locationRevealed attrs
    ]

instance RunMessage Gallery where
  runMessage msg l@(Gallery attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> l <$ push
      (beginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        2
      )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        clueCount <- field InvestigatorClues iid
        l <$ when
          (clueCount > 0)
          (pushAll [InvestigatorSpendClues iid 1, PlaceClues (toTarget attrs) 1]
          )
    _ -> Gallery <$> runMessage msg attrs
