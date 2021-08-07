module Arkham.Types.Asset.Cards.LadyEsprit
  ( LadyEsprit(..)
  , ladyEsprit
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Card.CardDef
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype LadyEsprit = LadyEsprit AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ladyEsprit :: AssetCard LadyEsprit
ladyEsprit = allyWith LadyEsprit Cards.ladyEsprit (2, 4) (isStoryL .~ True)

ability :: AssetAttrs -> Ability
ability attrs = (mkAbility
                  (toSource attrs)
                  1
                  (ActionAbility Nothing $ Costs
                    [ ActionCost 1
                    , ExhaustCost (toTarget attrs)
                    , HorrorCost (toSource attrs) (toTarget attrs) 1
                    ]
                  )
                )
  { abilityRestrictions = OnLocation <$> assetLocation attrs
  }

instance HasModifiersFor env LadyEsprit

instance HasActions env LadyEsprit where
  getActions _ NonFast (LadyEsprit a) = pure [ability a]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env LadyEsprit where
  runMessage msg a@(LadyEsprit attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ push
        (chooseOne
          iid
          [HealDamage (InvestigatorTarget iid) 2, TakeResources iid 2 False]
        )
    _ -> LadyEsprit <$> runMessage msg attrs
