{-# OPTIONS_GHC -Wno-deprecations #-}
module Arkham.Types.Treachery.Cards.CoverUp
  ( CoverUp(..)
  , coverUp
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype CoverUp = CoverUp TreacheryAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

coverUp :: TreacheryCard CoverUp
coverUp = treachery (CoverUp . (cluesL ?~ 3)) Cards.coverUp

coverUpClues :: TreacheryAttrs -> Int
coverUpClues TreacheryAttrs { treacheryClues } =
  fromJustNote "must be set" treacheryClues

instance HasModifiersFor env CoverUp where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CoverUp where
  getActions iid (WhenDiscoverClues You YourLocation) (CoverUp a) =
    withTreacheryInvestigator a $ \tormented -> do
      treacheryLocationId <- getId @LocationId tormented
      investigatorLocationId <- getId @LocationId iid
      cluesToDiscover <- fromQueue $ \queue -> do
        let
          mDiscoverClues = flip find queue $ \case
            DiscoverClues{} -> True
            _ -> False
        case mDiscoverClues of
          Just (DiscoverClues _ _ m _) -> m
          _ -> 0
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource a) 1 (ReactionAbility Free))
        | (treacheryLocationId == investigatorLocationId)
          && (coverUpClues a > 0)
          && (cluesToDiscover > 0)
        ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env CoverUp where
  runMessage msg t@(CoverUp attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ RemoveCardFromHand iid "01007"
      , AttachTreachery treacheryId (InvestigatorTarget iid)
      ]
    InvestigatorEliminated iid | treacheryOnInvestigator iid attrs ->
      runMessage EndOfGame t >>= \case
        CoverUp attrs' -> CoverUp <$> runMessage msg attrs'
    EndOfGame | coverUpClues attrs > 0 -> withTreacheryInvestigator attrs
      $ \tormented -> t <$ unshiftMessage (SufferTrauma tormented 0 1)
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      cluesToRemove <- withQueue $ \queue -> do
        let
          (before, after) = flip break queue $ \case
            DiscoverClues{} -> True
            _ -> False
          (DiscoverClues _ _ m _, remaining) = case after of
            [] -> error "DiscoverClues has to be present"
            (x : xs) -> (x, xs)
        (before <> remaining, m)
      let remainingClues = max 0 (coverUpClues attrs - cluesToRemove)
      pure $ CoverUp (attrs { treacheryClues = Just remainingClues })
    _ -> CoverUp <$> runMessage msg attrs
