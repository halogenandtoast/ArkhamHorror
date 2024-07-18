module Arkham.Event.Cards.Galvanize1 (
  galvanize1,
  Galvanize1 (..),
) where

import Arkham.Prelude

import Arkham.Action.Additional
import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype Galvanize1 = Galvanize1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

galvanize1 :: EventCard Galvanize1
galvanize1 = event Galvanize1 Cards.galvanize1

instance RunMessage Galvanize1 where
  runMessage msg e@(Galvanize1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      assets <- select $ assetControlledBy iid <> AssetExhausted <> AssetWithClass Guardian
      player <- getPlayer iid
      pushAll
        $ [ chooseOrRunOne player
            $ [targetLabel aid [Ready (AssetTarget aid)] | aid <- assets]
          | notNull assets
          ]
        <> [ turnModifier
              iid
              attrs
              iid
              (GiveAdditionalAction $ AdditionalAction "Galvanize" (toSource attrs) #fight)
           ]
      pure e
    _ -> Galvanize1 <$> runMessage msg attrs
