module Arkham.Treachery.Cards.YithianPresence (
  yithianPresence,
  YithianPresence (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype YithianPresence = YithianPresence TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

yithianPresence :: TreacheryCard YithianPresence
yithianPresence = treachery YithianPresence Cards.yithianPresence

instance HasModifiersFor YithianPresence where
  getModifiersFor (InvestigatorTarget iid) (YithianPresence a)
    | treacheryOnInvestigator iid a = do
        yithianPresent <-
          selectAny
            $ EnemyWithTrait Yithian
            <> EnemyAt
              (locationWithInvestigator iid)
        mlid <- selectOne $ locationWithInvestigator iid
        pure
          $ if yithianPresent
            then
              toModifiers a
                $ CannotTriggerAbilityMatching AbilityOnEncounterCard
                : [CannotInvestigateLocation lid | lid <- maybeToList mlid]
            else []
  getModifiersFor _ _ = pure []

instance HasAbilities YithianPresence where
  getAbilities (YithianPresence a) =
    [ restrictedAbility a 1 OnSameLocation
        $ ActionAbility []
        $ ActionCost 1
        <> HandDiscardCost 2 AnyCard
    ]

instance RunMessage YithianPresence where
  runMessage msg t@(YithianPresence attrs) = case msg of
    Revelation iid (isSource attrs -> True) ->
      t <$ push (AttachTreachery (toId t) (InvestigatorTarget iid))
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> YithianPresence <$> runMessage msg attrs
