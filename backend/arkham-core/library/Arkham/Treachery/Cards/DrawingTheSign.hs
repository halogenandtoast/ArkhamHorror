module Arkham.Treachery.Cards.DrawingTheSign
  ( drawingTheSign
  , DrawingTheSign(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Runner

newtype DrawingTheSign = DrawingTheSign TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawingTheSign :: TreacheryCard DrawingTheSign
drawingTheSign = treachery DrawingTheSign Cards.drawingTheSign

instance HasModifiersFor DrawingTheSign where
  getModifiersFor (InvestigatorTarget iid) (DrawingTheSign attrs) =
    pure $ toModifiers
      attrs
      [ HandSize (-5) | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ _ = pure []

instance HasAbilities DrawingTheSign where
  getAbilities (DrawingTheSign a) =
    [ restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost
        2
    ]

instance RunMessage DrawingTheSign where
  runMessage msg t@(DrawingTheSign attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> DrawingTheSign <$> runMessage msg attrs
