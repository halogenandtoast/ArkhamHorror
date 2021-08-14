module Arkham.Types.Asset.Cards.WhittonGreene
  ( whittonGreene
  , WhittonGreene(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.InvestigatorId
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype WhittonGreene = WhittonGreene AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whittonGreene :: AssetCard WhittonGreene
whittonGreene = ally WhittonGreene Cards.whittonGreene (2, 2)

instance HasActions WhittonGreene where
  getActions (WhittonGreene x) =
    [ restrictedAbility x 1 OwnsThis $ ReactionAbility
        (OrWindowMatcher
          [ RevealLocation Timing.After You Anywhere
          , PutLocationIntoPlay Timing.After You Anywhere
          ]
        )
        ExhaustThis
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
      (SearchTopOfDeck
        iid
        source
        (InvestigatorTarget iid)
        6
        [Tome, Relic]
        (ShuffleBackIn $ DrawFound iid)
      )
    _ -> WhittonGreene <$> runMessage msg attrs
