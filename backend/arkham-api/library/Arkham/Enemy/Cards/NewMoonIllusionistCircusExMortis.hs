module Arkham.Enemy.Cards.NewMoonIllusionistCircusExMortis (newMoonIllusionistCircusExMortis) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (findAllCards, getHistory)
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelf, modifySelfWhen)
import Arkham.History
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Hex, Woods))

newtype NewMoonIllusionist = NewMoonIllusionist EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

newMoonIllusionistCircusExMortis :: EnemyCard NewMoonIllusionist
newMoonIllusionistCircusExMortis =
  enemy NewMoonIllusionist Cards.newMoonIllusionistCircusExMortis

instance HasModifiersFor NewMoonIllusionist where
  getModifiersFor (NewMoonIllusionist a) = do
    modifySelf a [AddKeyword Keyword.Hunter, AddKeyword Keyword.Aloof]
    modifySelfWhen a (enemyExhausted a) [RemoveKeyword Keyword.Aloof]
    -- The first Hex treachery drawn each round by each investigator at this
    -- location and each connecting Woods location gains surge. Mirrors
    -- Crag of the Ghouls: apply Surge to all Hex treachery cards while some
    -- qualifying investigator's round history shows exactly one Hex treachery
    -- drawn (i.e. the one currently resolving is their first this round).
    -- ponytail: with multiple qualifying investigators this can grant surge to
    -- a later drawer's non-first Hex if another is sitting at exactly one; the
    -- card-targeted-modifier surge pattern cannot scope to the drawer. Same
    -- known ceiling as the official Crag of the Ghouls implementation.
    let
      locs =
        oneOf
          [ locationWithEnemy a
          , connectedTo (locationWithEnemy a) <> LocationWithTrait Woods
          ]
    hexCards <- findAllCards (`cardMatch` (CardWithType TreacheryType <> CardWithTrait Hex))
    let hexCodes = map toCardCode hexCards
    iids <- select (InvestigatorAt locs)
    firstDraws <- for iids \iid -> do
      h <- getHistory RoundHistory iid
      pure $ length (filter (`elem` hexCodes) (historyTreacheriesDrawn h)) == 1
    when (or firstDraws) do
      modifyEach a (map (CardIdTarget . toCardId) hexCards) [AddKeyword Keyword.Surge]

instance RunMessage NewMoonIllusionist where
  runMessage msg (NewMoonIllusionist attrs) =
    NewMoonIllusionist <$> runMessage msg attrs
