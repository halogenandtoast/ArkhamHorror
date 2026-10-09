module Arkham.Homebrew.TheSymphonyOfErichZann.Assets.LaFrattasPianoKey (laFrattasPianoKey) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype LaFrattasPianoKey = LaFrattasPianoKey AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laFrattasPianoKey :: AssetCard LaFrattasPianoKey
laFrattasPianoKey = asset LaFrattasPianoKey Cards.laFrattasPianoKey

instance HasAbilities LaFrattasPianoKey where
  {- At the end of a turn in which no action type was taken twice, exhaust for an
  extra action. The engine only reports a run of *different* types in a row, so
  the criterion is "every action this turn was a different type". -}
  getAbilities (LaFrattasPianoKey a) =
    [ controlled a 1 (youExist InvestigatorWithNoRepeatedActionsThisTurn)
        $ triggered (TurnEnds #when You) (exhaust a)
    ]

instance RunMessage LaFrattasPianoKey where
  runMessage msg a@(LaFrattasPianoKey attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      takeActionAsIfTurn iid (attrs.ability 1)
      pure a
    _ -> LaFrattasPianoKey <$> liftRunMessage msg attrs
