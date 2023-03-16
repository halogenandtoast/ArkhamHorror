module Arkham.Location.Cards.SanMarcoBasilica
  ( sanMarcoBasilica
  , SanMarcoBasilica(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types ( Field (..) )
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding ( PlaceUnderneath )
import Arkham.Message

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
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      innocentRevelers <-
        selectWithField AssetCardId
        $ AssetControlledBy You
        <> assetIs Assets.innocentReveler
      push $ chooseOne
        iid
        [ targetLabel
            innocentReveler
            [ PlaceUnderneath
              ActDeckTarget
              [PlayerCard $ lookupPlayerCard Assets.innocentReveler cardId]
            , RemoveFromGame (toTarget innocentReveler)
            ]
        | (innocentReveler, cardId) <- innocentRevelers
        ]
      pure l
    _ -> SanMarcoBasilica <$> runMessage msg attrs
