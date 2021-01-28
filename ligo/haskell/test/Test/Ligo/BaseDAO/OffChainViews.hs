-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.OffChainViews
  ( test_FA2
  ) where

import Universum hiding (view)

import qualified Data.Map as M
import Lorentz (BigMap(..))
import Named (defaults, (!))
import Test.Tasty (TestTree)

import Tezos.Address

import BaseDAO.ShareTest.Common (getTotalSupplyFromLedger)
import BaseDAO.ShareTest.OffChainViews
import qualified Ligo.BaseDAO.Types as DAO
import qualified Ligo.BaseDAO.TZIP16Metadata as DAO
import qualified Lorentz.Contracts.BaseDAO.TZIP16Metadata as DAO

defaultStorage :: DAO.StorageL
defaultStorage =
  (DAO.mkStorageL
  ! #admin addr
  ! #extra DAO.dynRecUnsafe
  ! #metadata mempty
  ! defaults
  ) { DAO.sLedger = bal
    , DAO.sTotalSupply = getTotalSupplyFromLedger bal
    }
  where
    addr = unsafeParseAddress "tz1M6dcor9QNTFr9Ri68cBYvpxrogZaMttuE"
    bal = BigMap $ M.fromList $
      [ ((addr, DAO.unfrozenTokenId), 100)
      , ((addr, DAO.frozenTokenId), 200)
      ]

test_FA2 :: TestTree
test_FA2 =
  mkFA2Tests defaultStorage (DAO.mkMetadataSettingsL DAO.defaultMetadataConfig)
