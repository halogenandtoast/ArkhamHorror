module Arkham.Treachery.Cards.HuntedByByakhee (huntedByByakhee, HuntedByByakhee (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Prelude
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype HuntedByByakhee = HuntedByByakhee TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntedByByakhee :: TreacheryCard HuntedByByakhee
huntedByByakhee = treachery HuntedByByakhee Cards.huntedByByakhee

instance RunMessage HuntedByByakhee where
  runMessage msg t@(HuntedByByakhee attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid source #agility (Fixed 6)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      pushAll
        [ DiscardTopOfEncounterDeck iid n (toSource attrs) (Just $ toTarget attrs)
        , ShuffleDeck Deck.EncounterDeck
        ]
      pure t
    DiscardedTopOfEncounterDeck iid cards _ target | isTarget attrs target -> do
      player <- getPlayer iid
      let
        isByakhee = member Byakhee . cdCardTraits . toCardDef
        isOmen = member Omen . cdCardTraits . toCardDef
        byakhee = filter isByakhee cards
        omens = filter isOmen cards
        byakheeMsgs =
          if null byakhee
            then []
            else
              [ FocusCards $ map EncounterCard cards
              , chooseOne
                  player
                  [ targetLabel
                    (toCardId enemy)
                    [InvestigatorDrewEncounterCard iid enemy]
                  | enemy <- byakhee
                  ]
              , UnfocusCards
              ]
      pushAll
        $ byakheeMsgs
        <> [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
           | not (null omens)
           ]
      pure t
    _ -> HuntedByByakhee <$> runMessage msg attrs
