module Arkham.Types.Asset.Cards.JoeyTheRatVigil
  ( joeyTheRatVigil
  , JoeyTheRatVigil(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Restriction hiding (DuringTurn, FastPlayerWindow)
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait
import Arkham.Types.Window

newtype JoeyTheRatVigil = JoeyTheRatVigil AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeyTheRatVigil :: AssetCard JoeyTheRatVigil
joeyTheRatVigil = ally JoeyTheRatVigil Cards.joeyTheRatVigil (3, 2)

-- This card is a pain and the solution here is a hack
-- we end up with a separate function for resource modification
instance HasActions JoeyTheRatVigil where
  getActions (JoeyTheRatVigil x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> PlayableCardExists
          (InHandOf You <> BasicCardMatch (CardWithTrait Item))
        )
        (FastAbility $ ResourceCost 1)
    ]

instance HasModifiersFor env JoeyTheRatVigil

instance CanCheckPlayable env => RunMessage env JoeyTheRatVigil where
  runMessage msg a@(JoeyTheRatVigil attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      handCards <- map unHandCard <$> getList iid
      let items = filter (member Item . toTraits) handCards
      playableItems <- filterM
        (getIsPlayable
          iid
          [ Window Timing.When (DuringTurn iid)
          , Window Timing.When FastPlayerWindow
          ]
        )
        items
      a <$ push
        (chooseOne
          iid
          [ Run
              [ PayCardCost iid (toCardId item)
              , InitiatePlayCard iid (toCardId item) Nothing False
              ]
          | item <- playableItems
          ]
        )
    _ -> JoeyTheRatVigil <$> runMessage msg attrs
