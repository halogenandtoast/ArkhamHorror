module Arkham.Asset.Cards.JoeyTheRatVigil
  ( joeyTheRatVigil
  , JoeyTheRatVigil(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria hiding ( DuringTurn )
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher hiding ( DuringTurn, FastPlayerWindow )
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window

newtype JoeyTheRatVigil = JoeyTheRatVigil AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeyTheRatVigil :: AssetCard JoeyTheRatVigil
joeyTheRatVigil = ally JoeyTheRatVigil Cards.joeyTheRatVigil (3, 2)

-- TODO: This does not account for the 1 resource spent in the cost
instance HasAbilities JoeyTheRatVigil where
  getAbilities (JoeyTheRatVigil x) =
    [ restrictedAbility
        x
        1
        (ControlsThis <> PlayableCardExists
          (InHandOf You <> BasicCardMatch (CardWithTrait Item))
        )
        (FastAbility $ ResourceCost 1)
    ]

instance RunMessage JoeyTheRatVigil where
  runMessage msg a@(JoeyTheRatVigil attrs) = case msg of
    UseCardAbility iid source 1 windows' _ | isSource attrs source -> do
      handCards <- field InvestigatorHand iid
      let items = filter (member Item . toTraits) handCards
          windows'' = nub $ windows' <>
            [ Window Timing.When (DuringTurn iid)
            , Window Timing.When FastPlayerWindow
            ]
      playableItems <- filterM
        (getIsPlayable
          iid
          source
          UnpaidCost
          windows''
        )
        items
      push $ chooseOne
        iid
        [ TargetLabel (CardIdTarget $ toCardId item) [PayCardCost iid item windows'']
        | item <- playableItems
        ]
      pure a
    _ -> JoeyTheRatVigil <$> runMessage msg attrs
