module Arkham.Treachery.Cards.AngeredSpirits (
  angeredSpirits,
  AngeredSpirits (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher hiding (FastPlayerWindow)
import Arkham.Message hiding (InvestigatorEliminated)
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype AngeredSpirits = AngeredSpirits TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

angeredSpirits :: TreacheryCard AngeredSpirits
angeredSpirits = treachery AngeredSpirits Cards.angeredSpirits

instance HasAbilities AngeredSpirits where
  getAbilities (AngeredSpirits a) =
    restrictedAbility
      a
      1
      OnSameLocation
      ( FastAbility $
          ExhaustAssetCost $
            AssetWithTrait Spell
              <> AssetControlledBy You
      )
      : [ restrictedAbility a 2 (ChargesOnThis $ EqualTo $ Static 0) $
          ForcedAbility $
            OrWindowMatcher
              [ GameEnds Timing.When
              , InvestigatorEliminated Timing.When (InvestigatorWithId iid)
              ]
        | iid <- maybeToList (treacheryOwner a)
        ]

instance RunMessage AngeredSpirits where
  runMessage msg t@(AngeredSpirits attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      pushAll
        [ RemoveCardFromHand iid (toCardId attrs)
        , AttachTreachery (toId attrs) (InvestigatorTarget iid)
        ]
      pure t
    UseCardAbility _ source 1 _ (ExhaustPayment [target])
      | isSource attrs source -> do
          pushAll
            [SpendUses target Charge 1, PlaceResources (toAbilitySource attrs 1) (toTarget attrs) 1]
          pure t
    UseCardAbility _ source 2 _ _ | isSource attrs source ->
      withTreacheryInvestigator attrs $
        \tormented -> t <$ push (SufferTrauma tormented 1 0)
    _ -> AngeredSpirits <$> runMessage msg attrs
