{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Capability where

import Arkham.GameValue
import Arkham.Id
import Arkham.Matcher.Patterns
import Arkham.Matcher.Types
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Target

class Capable a where
  can :: Capabilities a

data FromSource = FromPlayerCardEffect | FromOtherSource
  deriving stock (Eq)

instance Capable InvestigatorMatcher where
  can =
    Capabilities
      { search = SearchCapabilities {deck = InvestigatorCanSearchDeck}
      , manipulate = ManipulateCapabilities {deck = InvestigatorWithoutModifier CannotManipulateDeck}
      , shuffle = ManipulateCapabilities {deck = InvestigatorWithoutModifier CannotManipulateDeck}
      , draw =
          DrawCapabilities
            { cards =
                InvestigatorWithoutModifier CannotDrawCards
                  <> InvestigatorWithoutModifier CannotDrawCardsFromPlayerCardEffects
                  <> InvestigatorWithoutModifier CannotManipulateDeck
            }
      , gain =
          GainCapabilities
            { resources =
                InvestigatorWithoutModifier CannotGainResources
                  <> InvestigatorWithoutModifier CannotGainResourcesFromPlayerCardEffects
            }
      , spend = SpendCapabilities {resources = InvestigatorWithSpendableResources (GreaterThan $ Static 0)}
      , have =
          HaveCapabilities
            { cards =
                HaveCardsCapabilities {leaveDiscard = InvestigatorWithoutModifier CardsCannotLeaveYourDiscardPile}
            }
      , affect =
          AffectCapabilities
            { otherPlayers = InvestigatorWithoutModifier CannotAffectOtherPlayersWithPlayerEffectsExceptDamage
            }
      , move = InvestigatorWithoutModifier CannotMove
      , target = TargetCapabilities {encounterDeck = InvestigatorCanTarget EncounterDeckTarget}
      , reveal = RevealCapabilities {cards = InvestigatorWithoutModifier CannotRevealCards}
      }

instance Capable (InvestigatorMatcher -> InvestigatorMatcher) where
  can =
    let can' = can :: Capabilities InvestigatorMatcher
     in fmap (<>) can'

instance Capable (InvestigatorId -> InvestigatorMatcher) where
  can =
    let can' = can :: Capabilities InvestigatorMatcher
     in fmap (flip ((<>) . InvestigatorWithId)) can'

instance Capable (FromSource -> InvestigatorMatcher) where
  can =
    let can' = fmap const can
     in can'
          { draw =
              DrawCapabilities
                { cards = \case
                    FromPlayerCardEffect -> can.draw.cards
                    FromOtherSource ->
                      InvestigatorWithoutModifier CannotDrawCards
                        <> InvestigatorWithoutModifier CannotManipulateDeck
                }
          , gain =
              GainCapabilities
                { resources = \case
                    FromPlayerCardEffect -> can.gain.resources
                    FromOtherSource -> InvestigatorWithoutModifier CannotGainResources
                }
          }

instance Capable (FromSource -> InvestigatorMatcher -> InvestigatorMatcher) where
  can =
    let can' = can :: Capabilities (FromSource -> InvestigatorMatcher)
     in fmap
          (\(m :: FromSource -> InvestigatorMatcher) fSource matcher -> m fSource <> matcher)
          can'

data Capabilities a = Capabilities
  { search :: SearchCapabilities a
  , draw :: DrawCapabilities a
  , manipulate :: ManipulateCapabilities a
  , shuffle :: ManipulateCapabilities a
  , gain :: GainCapabilities a
  , spend :: SpendCapabilities a
  , have :: HaveCapabilities a
  , affect :: AffectCapabilities a
  , target :: TargetCapabilities a
  , move :: a
  , reveal :: RevealCapabilities a
  }
  deriving stock (Functor)

data AffectCapabilities a = AffectCapabilities
  { otherPlayers :: a
  }
  deriving stock (Functor)

data ManipulateCapabilities a = ManipulateCapabilities
  { deck :: a
  }
  deriving stock (Functor)

data SearchCapabilities a = SearchCapabilities
  { deck :: a
  }
  deriving stock (Functor)

data DrawCapabilities a = DrawCapabilities
  { cards :: a
  }
  deriving stock (Functor)

data RevealCapabilities a = RevealCapabilities
  { cards :: a
  }
  deriving stock (Functor)

data GainCapabilities a = GainCapabilities
  { resources :: a
  }
  deriving stock (Functor)

data SpendCapabilities a = SpendCapabilities
  { resources :: a
  }
  deriving stock (Functor)

data HaveCapabilities a = HaveCapabilities
  { cards :: HaveCardsCapabilities a
  }
  deriving stock (Functor)

data HaveCardsCapabilities a = HaveCardsCapabilities
  { leaveDiscard :: a
  }
  deriving stock (Functor)

data TargetCapabilities a = TargetCapabilities
  { encounterDeck :: a
  }
  deriving stock (Functor)
