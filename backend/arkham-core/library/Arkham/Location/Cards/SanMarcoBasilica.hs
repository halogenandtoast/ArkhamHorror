module Arkham.Location.Cards.SanMarcoBasilica
  ( sanMarcoBasilica
  , SanMarcoBasilica(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding ( PlaceUnderneath )
import Arkham.Message
import Arkham.Target

newtype SanMarcoBasilica = SanMarcoBasilica LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanMarcoBasilica :: LocationCard SanMarcoBasilica
sanMarcoBasilica = locationWith
  SanMarcoBasilica
  Cards.sanMarcoBasilica
  3
  (Static 0)
  (connectsToL .~ singleton RightOf)

instance HasAbilities SanMarcoBasilica where
  getAbilities (SanMarcoBasilica attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            (Here <> AssetExists
              (AssetControlledBy You <> assetIs Assets.innocentReveler)
            )
          $ ActionAbility Nothing
          $ ActionCost 1
        | locationRevealed attrs
        ]

instance RunMessage SanMarcoBasilica where
  runMessage msg l@(SanMarcoBasilica attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      innocentRevelerIds <-
        selectList $ AssetControlledBy You <> assetIs Assets.innocentReveler
      l <$ push
        (chooseOne
          iid
          [ TargetLabel
              (AssetTarget innocentRevelerId)
              [ PlaceUnderneath
                ActDeckTarget
                [ PlayerCard $ lookupPlayerCard
                    Assets.innocentReveler
                    (unAssetId innocentRevelerId)
                ]
              , RemoveFromGame (AssetTarget innocentRevelerId)
              ]
          | innocentRevelerId <- innocentRevelerIds
          ]
        )
    _ -> SanMarcoBasilica <$> runMessage msg attrs
