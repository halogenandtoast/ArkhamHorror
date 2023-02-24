module Arkham.Treachery.Cards.CoverUp
  ( CoverUp(..)
  , coverUp
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Matcher hiding ( DiscoverClues )
import Arkham.Matcher qualified as Matcher
import Arkham.Message hiding ( InvestigatorEliminated )
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CoverUp = CoverUp TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

coverUp :: TreacheryCard CoverUp
coverUp = treacheryWith CoverUp Cards.coverUp (cluesL .~ 3)

instance HasAbilities CoverUp where
  getAbilities (CoverUp a) =
    restrictedAbility
        a
        1
        (OnSameLocation <> CluesOnThis (AtLeast $ Static 1))
        (ReactionAbility
          (Matcher.DiscoverClues Timing.When You YourLocation $ AtLeast $ Static
            1
          )
          Free
        )
      : [ restrictedAbility a 2 (CluesOnThis $ AtLeast $ Static 1)
          $ ForcedAbility
          $ OrWindowMatcher
              [ GameEnds Timing.When
              , InvestigatorEliminated Timing.When (InvestigatorWithId iid)
              ]
        | iid <- maybeToList (treacheryOwner a)
        ]

instance RunMessage CoverUp where
  runMessage msg t@(CoverUp attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid (isSource attrs -> True) ->
      t <$ push (AttachTreachery treacheryId (InvestigatorTarget iid))
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      mMsg <- popMessageMatching $ \case
        Do (InvestigatorDiscoverClues{}) -> True
        _ -> False
      case mMsg of
        Just (Do (InvestigatorDiscoverClues _ _ cluesToRemove _)) ->
          pure $ CoverUp $ attrs & cluesL %~ max 0 . subtract cluesToRemove
        _ -> error "Do InvestigatorDiscoverClues  has to be present"
    UseCardAbility _ source 2 _ _ | isSource attrs source ->
      withTreacheryInvestigator attrs
        $ \tormented -> t <$ push (SufferTrauma tormented 0 1)
    _ -> CoverUp <$> runMessage msg attrs
