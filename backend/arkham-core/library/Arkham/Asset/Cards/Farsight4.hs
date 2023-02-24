module Arkham.Asset.Cards.Farsight4
  ( farsight4
  , Farsight4(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher hiding ( DuringTurn )
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype Farsight4 = Farsight4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

farsight4 :: AssetCard Farsight4
farsight4 = asset Farsight4 Cards.farsight4

instance HasAbilities Farsight4 where
  getAbilities (Farsight4 a) =
    [ restrictedAbility
          a
          1
          (ControlsThis
          <> DuringTurn You
          <> InvestigatorExists
               (You <> HandWith (LengthIs $ AtLeast $ Static 8))
          <> PlayableCardExists
               (InHandOf You <> BasicCardMatch (CardWithType EventType))
          )
        $ FastAbility
        $ ExhaustCost
        $ toTarget a
    ]

instance RunMessage Farsight4 where
  runMessage msg a@(Farsight4 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      handCards <- field InvestigatorHand iid
      let
        events = filter ((== EventType) . cdCardType . toCardDef) handCards
        windows'' =
          nub
            $ windows'
            <> [ Window Timing.When (Window.DuringTurn iid)
               , Window Timing.When Window.FastPlayerWindow
               ]
      playableEvents <- filterM
        (getIsPlayable iid (toSource attrs) UnpaidCost windows'')
        events
      push $ chooseOne
        iid
        [ TargetLabel
            (CardIdTarget $ toCardId event)
            [PayCardCost iid event windows'']
        | event <- playableEvents
        ]
      pure a
    _ -> Farsight4 <$> runMessage msg attrs
