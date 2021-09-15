module Arkham.Types.Asset.Cards.MrPeabody
  ( mrPeabody
  , MrPeabody(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype MrPeabody = MrPeabody AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mrPeabody :: AssetCard MrPeabody
mrPeabody = ally MrPeabody Cards.mrPeabody (2, 2)

instance HasAbilities MrPeabody where
  getAbilities (MrPeabody attrs) =
    [ restrictedAbility attrs 1 OwnsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, ExhaustCost $ toTarget attrs]
    ]

instance AssetRunner env => RunMessage env MrPeabody where
  runMessage msg a@(MrPeabody attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locations <- selectListMap LocationTarget Anywhere
      a <$ push
        (chooseOne
          iid
          [ TargetLabel
              target
              [CreateEffect (toCardCode attrs) Nothing source target]
          | target <- locations
          ]
        )
    _ -> MrPeabody <$> runMessage msg attrs
