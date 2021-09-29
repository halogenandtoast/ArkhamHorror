module Arkham.Types.Asset.Cards.WhittonGreene
  ( whittonGreene
  , WhittonGreene(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.InvestigatorId
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait
import Arkham.Types.Zone

newtype WhittonGreene = WhittonGreene AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whittonGreene :: AssetCard WhittonGreene
whittonGreene = ally WhittonGreene Cards.whittonGreene (2, 2)

instance HasAbilities WhittonGreene where
  getAbilities (WhittonGreene x) =
    [ restrictedAbility x 1 OwnsThis $ ReactionAbility
        (OrWindowMatcher
          [ RevealLocation Timing.After You Anywhere
          , PutLocationIntoPlay Timing.After You Anywhere
          ]
        )
        (ExhaustCost $ toTarget x)
    ]

instance HasCount AssetCount env (InvestigatorId, [Trait]) => HasModifiersFor env WhittonGreene where
  getModifiersFor _ (InvestigatorTarget iid) (WhittonGreene a) | ownedBy a iid =
    do
      active <- (> 0) . unAssetCount <$> getCount (iid, [Tome, Relic])
      pure $ toModifiers a [ SkillModifier SkillIntellect 1 | active ]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env WhittonGreene where
  runMessage msg a@(WhittonGreene attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (Search
        iid
        source
        (InvestigatorTarget iid)
        (FromTopOfDeck 6)
        [Tome, Relic]
        (ShuffleBackIn $ DrawFound iid)
      )
    _ -> WhittonGreene <$> runMessage msg attrs
