module Arkham.Types.Location.Cards.SanMarcoBasilica
  ( sanMarcoBasilica
  , SanMarcoBasilica(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher hiding (PlaceUnderneath)
import Arkham.Types.Message
import Arkham.Types.Target

newtype SanMarcoBasilica = SanMarcoBasilica LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanMarcoBasilica :: LocationCard SanMarcoBasilica
sanMarcoBasilica = locationWith
  SanMarcoBasilica
  Cards.sanMarcoBasilica
  3
  (Static 0)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasAbilities SanMarcoBasilica where
  getAbilities (SanMarcoBasilica attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            (Here <> AssetExists
              (AssetOwnedBy You <> assetIs Assets.innocentReveler)
            )
          $ ActionAbility Nothing
          $ ActionCost 1
        | locationRevealed attrs
        ]

instance LocationRunner env => RunMessage env SanMarcoBasilica where
  runMessage msg l@(SanMarcoBasilica attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      innocentRevelerIds <-
        selectList $ AssetOwnedBy You <> assetIs Assets.innocentReveler
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
