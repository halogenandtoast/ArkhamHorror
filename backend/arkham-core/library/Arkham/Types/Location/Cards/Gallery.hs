module Arkham.Types.Location.Cards.Gallery
  ( gallery
  , Gallery(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype Gallery = Gallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gallery :: LocationCard Gallery
gallery = location Gallery Cards.gallery 1 (Static 0) Plus [Equals, Circle]

instance HasAbilities Gallery where
  getAbilities (Gallery attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here $ ForcedAbility $ TurnEnds Timing.After You
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env Gallery where
  runMessage msg l@(Gallery attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        2
      )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        clueCount <- unClueCount <$> getCount iid
        l <$ when
          (clueCount > 0)
          (pushAll [InvestigatorSpendClues iid 1, PlaceClues (toTarget attrs) 1]
          )
    _ -> Gallery <$> runMessage msg attrs
