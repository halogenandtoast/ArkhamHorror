module Arkham.Story.Cards.WaresOfBaharna (WaresOfBaharna (..), waresOfBaharna) where

import Arkham.Capability
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Trait (Trait (Item, Supply))

newtype WaresOfBaharna = WaresOfBaharna StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

waresOfBaharna :: StoryCard WaresOfBaharna
waresOfBaharna = story WaresOfBaharna Cards.waresOfBaharna

instance RunMessage WaresOfBaharna where
  runMessage msg s@(WaresOfBaharna attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      investigators <-
        selectList
          $ at_ (locationIs Locations.baharna)
          <> DiscardWith (HasCard $ hasAnyTrait [Item, Supply])
          <> can.have.cards.leaveDiscard

      msgPairs <- forToSnd investigators $ \iid -> do
        player <- getPlayer iid
        discards <- fieldMap InvestigatorDiscard (map toCard) iid
        let targets = filter (`cardMatch` hasAnyTrait @CardMatcher [Item, Supply]) discards
        pure
          [ FocusCards discards
          , chooseOne
              player
              $ Label "Do not return card" []
              : [targetLabel (toCardId card) [AddToHand iid [card], UnfocusCards] | card <- targets]
          ]

      lead <- getLeadPlayer
      pushAll
        [ chooseOrRunOneAtATimeWithLabel "Choose investigator to decide on returning discard" lead
            $ map (uncurry targetLabel) msgPairs
        , Remember ObtainedSuppliesFromBaharna
        ]

      pure s
    _ -> WaresOfBaharna <$> runMessage msg attrs
