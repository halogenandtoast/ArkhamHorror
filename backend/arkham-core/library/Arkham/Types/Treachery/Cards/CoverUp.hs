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
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.Window

newtype CoverUp = CoverUp TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

coverUp :: TreacheryCard CoverUp
coverUp = treacheryWith CoverUp Cards.coverUp (cluesL ?~ 3)

coverUpClues :: TreacheryAttrs -> Int
coverUpClues TreacheryAttrs { treacheryClues } =
  fromJustNote "must be set" treacheryClues

instance HasModifiersFor env CoverUp

instance ActionRunner env => HasAbilities env CoverUp where
  getAbilities iid (WhenDiscoverClues who lid n) (CoverUp a) | iid == who =
    withTreacheryInvestigator a $ \tormented -> do
      treacheryLocationId <- getId @LocationId tormented
      pure
        [ mkAbility a 1 $ ResponseAbility Free
        | (treacheryLocationId == lid) && (coverUpClues a > 0) && (n > 0)
        ]
  getAbilities _ _ _ = pure []

instance TreacheryRunner env => RunMessage env CoverUp where
  runMessage msg t@(CoverUp attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ pushAll
      [ RemoveCardFromHand iid (toCardId attrs)
      , AttachTreachery treacheryId (InvestigatorTarget iid)
      ]
    InvestigatorEliminated iid | treacheryOnInvestigator iid attrs ->
      runMessage EndOfGame t >>= \case
        CoverUp attrs' -> CoverUp <$> runMessage msg attrs'
    EndOfGame | coverUpClues attrs > 0 -> withTreacheryInvestigator attrs
      $ \tormented -> t <$ push (SufferTrauma tormented 0 1)
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
