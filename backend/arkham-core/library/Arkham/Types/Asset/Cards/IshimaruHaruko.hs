module Arkham.Types.Asset.Cards.IshimaruHaruko
  ( ishimaruHaruko
  , IshimaruHaruko(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Story.Cards qualified as Story
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype IshimaruHaruko = IshimaruHaruko AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ishimaruHaruko :: AssetCard IshimaruHaruko
ishimaruHaruko = asset IshimaruHaruko Cards.ishimaruHaruko

instance HasAbilities IshimaruHaruko where
  getAbilities (IshimaruHaruko a) =
    [ restrictedAbility
        a
        1
        (OnSameLocation <> InvestigatorExists
          (You <> HandWith (LengthIs $ AtLeast $ Static 6))
        )
      $ ActionAbility Nothing
      $ ActionCost 1
    , mkAbility a 2
      $ ForcedAbility
      $ LastClueRemovedFromAsset Timing.When
      $ AssetWithId
      $ toId a
    ]

instance AssetRunner env => RunMessage env IshimaruHaruko where
  runMessage msg a@(IshimaruHaruko attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (BeginSkillTest iid source (toTarget attrs) Nothing SkillWillpower 2)
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        modifiers <- getModifiers source (InvestigatorTarget iid)
        a <$ when
          (assetClues attrs > 0 && CannotTakeControlOfClues `notElem` modifiers)
          (pushAll [RemoveClues (toTarget attrs) 1, GainClues iid 1])
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      a <$ push
        (ReadStory
        $ EncounterCard
        $ lookupEncounterCard Story.thePattern
        $ toCardId attrs
        )
    _ -> IshimaruHaruko <$> runMessage msg attrs
